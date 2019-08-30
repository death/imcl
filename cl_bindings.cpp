#include "imgui.h"
#include <ecl/ecl.h>

// Support

#define LENGTHOF(array) (sizeof(array) / sizeof((array)[0]))

static const char* as_text(cl_object obj)
{
    return ecl_base_string_pointer_safe(si_coerce_to_base_string(cl_princ_to_string(obj)));
}

static int as_int(cl_object obj)
{
    return ecl_to_int32_t(ecl_truncate1(obj));
}

static unsigned int as_uint(cl_object obj)
{
    return ecl_to_uint32_t(ecl_truncate1(obj));
}

static float as_float(cl_object obj)
{
    return ecl_to_float(obj);
}

static bool as_bool(cl_object obj)
{
    return !Null(obj);
}

static cl_object as_object(cl_object obj)
{
    return obj;
}

#define POPARG(fn, def) (nargs > 0 ? nargs--, fn(ecl_va_arg(ap)) : (def))

#define RETFLOAT(x) result = ecl_make_single_float((x))

#define RETINT(x) result = ecl_make_int32_t((x))

#define RETBOOL(x) result = (x) ? ECL_T : ECL_NIL

#define APIFUNC(name)                                   \
    static cl_object clapi_ ## name(cl_narg nargs, ...) \
    {                                                   \
    ecl_va_list ap;                                     \
    cl_object result = ECL_NIL;                         \
    ecl_va_start(ap, nargs, nargs, 0);

#define APIFUNC_END                             \
    ecl_va_end(ap);                             \
    ecl_return1(ecl_process_env(), result);     \
    }

// Translating keywords to enum values

struct keyword_enum_descriptor
{
    const char *keyword_name;
    int value;
};

int keyword_enum_value(cl_object keyword,
                       struct keyword_enum_descriptor *descriptors,
                       int size)
{
    if (!ecl_keywordp(keyword)) {
        FEerror("object supplied is not a keyword", 0);
    }
    for (int i = 0; i < size; i++) {
        if (ecl_equal(ecl_make_keyword(descriptors[i].keyword_name), keyword)) {
            return descriptors[i].value;
        }
    }
    FEerror("keyword not in enum descriptors list", 0);
    return 0;
}

int flags_enum_value(cl_object flags, struct keyword_enum_descriptor *descriptors, int size)
{
    if (ecl_keywordp(flags)) {
        return keyword_enum_value(flags, descriptors, size);
    } else if (cl_consp(flags) != ECL_NIL) {
        int value = 0;
        while (cl_consp(flags) != ECL_NIL) {
            cl_object item = cl_car(flags);
            value |= keyword_enum_value(item, descriptors, size);
            flags = cl_cdr(flags);
        }
        return value;
    } else {
        FEerror("expected keyword or list of keywords", 0);
        return 0;
    }
}

// Convenience

bool ecl_imvec2_p(cl_object object, ImVec2 & result)
{
    if (cl_consp(object) == ECL_NIL) {
        return false;
    }
    cl_object car = cl_car(object);
    if (!ecl_realp(car)) {
        return false;
    }
    cl_object cdr = cl_cdr(object);
    if (cl_consp(object) == ECL_NIL) {
        return false;
    }
    cl_object cadr = cl_car(cdr);
    if (!ecl_realp(cadr)) {
        return false;
    }
    cl_object cddr = cl_cdr(cdr);
    if (cl_null(cddr) == ECL_NIL) {
        return false;
    }
    result.x = ecl_to_float(car);
    result.y = ecl_to_float(cadr);
    return true;
}

ImVec2 as_imvec2(cl_object obj)
{
    ImVec2 imvec(0, 0);
    ecl_imvec2_p(obj, imvec);
    return imvec;
}

struct keyword_enum_descriptor imguicond[] = {
    {"ALWAYS", ImGuiCond_Always},
    {"ONCE", ImGuiCond_Once},
    {"FIRST-USE-EVER", ImGuiCond_FirstUseEver},
    {"APPEARING", ImGuiCond_Appearing},
};

ImGuiCond as_imguicond(cl_object obj)
{
    ImGuiCond cond = ImGuiCond_Always;
    if (obj != ECL_NIL) {
        cond = keyword_enum_value(obj, imguicond, LENGTHOF(imguicond));
    }
    return cond;
}

struct keyword_enum_descriptor imguicol[] = {
    {"TEXT", ImGuiCol_Text},
    {"TEXT-DISABLED", ImGuiCol_TextDisabled},
    {"WINDOW-BG", ImGuiCol_WindowBg},
    {"CHILD-BG", ImGuiCol_ChildBg},
    {"POPUP-BG", ImGuiCol_PopupBg},
    {"BORDER", ImGuiCol_Border},
    {"BORDER-SHADOW", ImGuiCol_BorderShadow},
    {"FRAME-BG", ImGuiCol_FrameBg},
    {"FRAME-BG-HOVERED", ImGuiCol_FrameBgHovered},
    {"FRAME-BG-ACTIVE", ImGuiCol_FrameBgActive},
    {"TITLE-BG", ImGuiCol_TitleBg},
    {"TITLE-BG-ACTIVE", ImGuiCol_TitleBgActive},
    {"TITLE-BG-COLLAPSED", ImGuiCol_TitleBgCollapsed},
    {"MENUBAR-BG", ImGuiCol_MenuBarBg}, // Unlike imgui we say "menubar" not "menu bar"
    {"SCROLLBAR-BG", ImGuiCol_ScrollbarBg},
    {"SCROLLBAR-GRAB", ImGuiCol_ScrollbarGrab},
    {"SCROLLBAR-GRAB-HOVERED", ImGuiCol_ScrollbarGrabHovered},
    {"SCROLLBAR-GRAB-ACTIVE", ImGuiCol_ScrollbarGrabActive},
    {"CHECKMARK", ImGuiCol_CheckMark}, // Unlike imgui we say "checkmark" not "check mark"
    {"SLIDER-GRAB", ImGuiCol_SliderGrab},
    {"SLIDER-GRAB-ACTIVE", ImGuiCol_SliderGrabActive},
    {"BUTTON", ImGuiCol_Button},
    {"BUTTON-HOVERED", ImGuiCol_ButtonHovered},
    {"BUTTON-ACTIVE", ImGuiCol_ButtonActive},
    {"HEADER", ImGuiCol_Header},
    {"HEADER-HOVERED", ImGuiCol_HeaderHovered},
    {"HEADER-ACTIVE", ImGuiCol_HeaderActive},
    {"SEPARATOR", ImGuiCol_Separator},
    {"SEPARATOR-HOVERED", ImGuiCol_SeparatorHovered},
    {"SEPARATOR-ACTIVE", ImGuiCol_SeparatorActive},
    {"RESIZE-GRIP", ImGuiCol_ResizeGrip},
    {"RESIZE-GRIP-HOVERED", ImGuiCol_ResizeGripHovered},
    {"RESIZE-GRIP-ACTIVE", ImGuiCol_ResizeGripActive},
    {"TAB", ImGuiCol_Tab},
    {"TAB-HOVERED", ImGuiCol_TabHovered},
    {"TAB-ACTIVE", ImGuiCol_TabActive},
    {"TAB-UNFOCUSED", ImGuiCol_TabUnfocused},
    {"TAB-UNFOCUSED-ACTIVE", ImGuiCol_TabUnfocusedActive},
    {"PLOT-LINES", ImGuiCol_PlotLines},
    {"PLOT-LINES-HOVERED", ImGuiCol_PlotLinesHovered},
    {"PLOT-HISTOGRAM", ImGuiCol_PlotHistogram},
    {"PLOT-HISTOGRAM-HOVERED", ImGuiCol_PlotHistogramHovered},
    {"TEXT-SELECTED-BG", ImGuiCol_TextSelectedBg},
    {"DRAG-DROP-TARGET", ImGuiCol_DragDropTarget},
    {"NAV-HIGHLIGHT", ImGuiCol_NavHighlight},
    {"NAV-WINDOWING-HIGHLIGHT", ImGuiCol_NavWindowingHighlight},
    {"NAV-WINDOWING-DIM-BG", ImGuiCol_NavWindowingDimBg},
    {"MODAL-WINDOW-DIM-BG", ImGuiCol_ModalWindowDimBg},
};

ImGuiCol as_imguicol(cl_object obj)
{
    ImGuiCol col = ImGuiCol_Text;
    if (obj != ECL_NIL) {
        col = keyword_enum_value(obj, imguicol, LENGTHOF(imguicol));
    }
    return col;
}


// The actual bindings

APIFUNC(begin)
{
    const char *label = POPARG(as_text, "Window");
    bool ret = ImGui::Begin(label);
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(end)
{
    ImGui::End();
}
APIFUNC_END

APIFUNC(text)
{
    const char *label = POPARG(as_text, "Text");
    ImGui::Text(label);
}
APIFUNC_END

APIFUNC(button)
{
    const char *label = POPARG(as_text, "Button");
    ImVec2 size = POPARG(as_imvec2, ImVec2(0, 0));
    bool ret = ImGui::Button(label, size);
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(sameline)
{
    ImGui::SameLine();
}
APIFUNC_END

APIFUNC(separator)
{
    ImGui::Separator();
}
APIFUNC_END

APIFUNC(begingroup)
{
    ImGui::BeginGroup();
}
APIFUNC_END

APIFUNC(endgroup)
{
    ImGui::EndGroup();
}
APIFUNC_END

APIFUNC(pushitemwidth)
{
    int width = POPARG(as_int, -1);
    ImGui::PushItemWidth(width);
}
APIFUNC_END

APIFUNC(popitemwidth)
{
    ImGui::PopItemWidth();
}
APIFUNC_END

APIFUNC(collapsingheader)
{
    const char *label = POPARG(as_text, "CollapsingHeader");
    bool ret = ImGui::CollapsingHeader(label);
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(spacing)
{
    ImGui::Spacing();
}
APIFUNC_END

APIFUNC(bullettext)
{
    const char *label = POPARG(as_text, "BulletText");
    ImGui::BulletText(label);
}
APIFUNC_END

APIFUNC(treenode)
{
    const char *label = POPARG(as_text, "TreeNode");
    bool ret = ImGui::TreeNode(label);
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(treepop)
{
    ImGui::TreePop();
}
APIFUNC_END

APIFUNC(setnextitemwidth)
{
    int width = POPARG(as_int, -1);
    ImGui::SetNextItemWidth(width);
}
APIFUNC_END

APIFUNC(dragfloat)
{
    const char *label = POPARG(as_text, "float");
    float f = POPARG(as_float, 0.0F);
    float v_speed = POPARG(as_float, 1.0F);
    float v_min = POPARG(as_float, 0.0F);
    float v_max = POPARG(as_float, 0.0F);
    const char *format = POPARG(as_text, "%.3f");
    float power = POPARG(as_float, 1.0);
    ImGui::DragFloat(label, &f, v_speed, v_min, v_max, format, power);
    RETFLOAT(f);
}
APIFUNC_END

enum cl_stylevar_type
{
    STYLEVAR_TYPE_FLOAT,
    STYLEVAR_TYPE_IMVEC2,
};

struct cl_stylevar_entry
{
    cl_object symbol;
    int idx;
    int type;
};

struct cl_stylevar_entry *stylevarlist()
{
    static struct cl_stylevar_entry list[ImGuiStyleVar_COUNT];
    static bool init = true;

    if (init) {
        list[0].symbol = ecl_make_keyword("ALPHA");
        list[0].type = STYLEVAR_TYPE_FLOAT;
        list[1].symbol = ecl_make_keyword("WINDOW-PADDING");
        list[1].type = STYLEVAR_TYPE_IMVEC2;
        list[2].symbol = ecl_make_keyword("WINDOW-ROUNDING");
        list[2].type = STYLEVAR_TYPE_FLOAT;
        list[3].symbol = ecl_make_keyword("WINDOW-BORDER-SIZE");
        list[3].type = STYLEVAR_TYPE_FLOAT;
        list[4].symbol = ecl_make_keyword("WINDOW-MIN-SIZE");
        list[4].type = STYLEVAR_TYPE_IMVEC2;
        list[5].symbol = ecl_make_keyword("WINDOW-TITLE-ALIGN");
        list[5].type = STYLEVAR_TYPE_IMVEC2;
        list[6].symbol = ecl_make_keyword("CHILD-ROUNDING");
        list[6].type = STYLEVAR_TYPE_FLOAT;
        list[7].symbol = ecl_make_keyword("CHILD-BORDER-SIZE");
        list[7].type = STYLEVAR_TYPE_FLOAT;
        list[8].symbol = ecl_make_keyword("POPUP-ROUNDING");
        list[8].type = STYLEVAR_TYPE_FLOAT;
        list[9].symbol = ecl_make_keyword("POPUP-BORDER-SIZE");
        list[9].type = STYLEVAR_TYPE_FLOAT;
        list[10].symbol = ecl_make_keyword("FRAME-PADDING");
        list[10].type = STYLEVAR_TYPE_IMVEC2;
        list[11].symbol = ecl_make_keyword("FRAME-ROUNDING");
        list[11].type = STYLEVAR_TYPE_FLOAT;
        list[12].symbol = ecl_make_keyword("FRAME-BORDER-SIZE");
        list[12].type = STYLEVAR_TYPE_FLOAT;
        list[13].symbol = ecl_make_keyword("ITEM-SPACING");
        list[13].type = STYLEVAR_TYPE_IMVEC2;
        list[14].symbol = ecl_make_keyword("ITEM-INNER-SPACING");
        list[14].type = STYLEVAR_TYPE_IMVEC2;
        list[15].symbol = ecl_make_keyword("INDENT-SPACING");
        list[15].type = STYLEVAR_TYPE_FLOAT;
        list[16].symbol = ecl_make_keyword("SCROLL-BAR-SIZE");
        list[16].type = STYLEVAR_TYPE_FLOAT;
        list[17].symbol = ecl_make_keyword("SCROLL-BAR-ROUNDING");
        list[17].type = STYLEVAR_TYPE_FLOAT;
        list[18].symbol = ecl_make_keyword("GRAB-MIN-SIZE");
        list[18].type = STYLEVAR_TYPE_FLOAT;
        list[19].symbol = ecl_make_keyword("GRAB-ROUNDING");
        list[19].type = STYLEVAR_TYPE_FLOAT;
        list[20].symbol = ecl_make_keyword("TAB-ROUNDING");
        list[20].type = STYLEVAR_TYPE_FLOAT;
        list[21].symbol = ecl_make_keyword("BUTTON-TEXT-ALIGN");
        list[21].type = STYLEVAR_TYPE_IMVEC2;
        list[22].symbol = ecl_make_keyword("SELECTABLE-TEXT-ALIGN");
        list[22].type = STYLEVAR_TYPE_IMVEC2;

        for (int i = 0; i < ImGuiStyleVar_COUNT; i++) {
            list[i].idx = i;
        }

        init = false;
    }

    return list;
}

APIFUNC(pushstylevar)
{
    cl_object whichvar = POPARG(as_object, ECL_NIL);
    if (ecl_keywordp(whichvar)) {
        struct cl_stylevar_entry *list = stylevarlist();
        int i;
        for (i = 0; i < ImGuiStyleVar_COUNT; i++) {
            if (ecl_equal(list[i].symbol, whichvar)) {
                break;
            }
        }
        if (i < ImGuiStyleVar_COUNT) {
            cl_object lispval = POPARG(as_object, ECL_NIL);
            ImVec2 vecval(0, 0);
            if (ecl_imvec2_p(lispval, vecval)) {
                if (list[i].type == STYLEVAR_TYPE_IMVEC2) {
                    ImGui::PushStyleVar(list[i].idx, vecval);
                } else {
                    FEerror("value is not an imvec2", 0);
                }
            } else if (ecl_realp(lispval)) {
                if (list[i].type == STYLEVAR_TYPE_FLOAT) {
                    float fval = ecl_to_float(lispval);
                    ImGui::PushStyleVar(list[i].idx, fval);
                } else {
                    FEerror("value is not a float", 0);
                }
            } else {
                FEerror("value is not an imvec2 or a float", 0);
            }
        } else {
            FEerror("unexpected style var name", 0);
        }
    } else {
        FEerror("var name is not a keyword", 0);
    }
}
APIFUNC_END

APIFUNC(popstylevar)
{
    int count = POPARG(as_int, 1);
    ImGui::PopStyleVar(count);
}
APIFUNC_END

APIFUNC(columns)
{
    int count = POPARG(as_int, 1);
    const char *id = POPARG(as_text, NULL);
    bool border = POPARG(as_bool, true);
    ImGui::Columns(count, id, border);
}
APIFUNC_END

APIFUNC(pushid)
{
    cl_object id = POPARG(as_object, ECL_NIL);
    if (ecl_realp(id)) {
        int idint = as_int(id);
        ImGui::PushID(idint);
    } else if (ecl_stringp(id)) {
        const char *idstring = as_text(id);
        ImGui::PushID(idstring);
    } else {
        FEerror("ID must be of type (OR STRING FIXNUM)", 0);
    }
}
APIFUNC_END

APIFUNC(popid)
{
    ImGui::PopID();
}
APIFUNC_END

APIFUNC(aligntexttoframepadding)
{
    ImGui::AlignTextToFramePadding();
}
APIFUNC_END

APIFUNC(nextcolumn)
{
    ImGui::NextColumn();
}
APIFUNC_END

APIFUNC(inputfloat)
{
    const char *label = POPARG(as_text, "label");
    float v = POPARG(as_float, 0.0F);
    float step = POPARG(as_float, 0.0F);
    float step_fast = POPARG(as_float, 0.0F);
    const char *format = POPARG(as_text, "%.3f");
    int flags = POPARG(as_int, 0);
    ImGui::InputFloat(label, &v, step, step_fast, format, flags);
    RETFLOAT(v);
}
APIFUNC_END

APIFUNC(setnextwindowsize)
{
    ImVec2 size = POPARG(as_imvec2, ImVec2(0, 0));
    ImGuiCond cond = POPARG(as_imguicond, ImGuiCond_Always);
    ImGui::SetNextWindowSize(size, cond);
}
APIFUNC_END

APIFUNC(beginchild)
{
    const char *id = POPARG(as_text, "child");
    ImVec2 size = POPARG(as_imvec2, ImVec2(0, 0));
    bool border = POPARG(as_bool, false);
    // TODO: flags arg
    ImGuiWindowFlags flags = 0;
    bool ret = ImGui::BeginChild(id, size, border, flags);
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(endchild)
{
    ImGui::EndChild();
}
APIFUNC_END

APIFUNC(selectable)
{
    const char *label = POPARG(as_text, "selectable");
    bool selected = POPARG(as_bool, false);
    ImGuiSelectableFlags flags = 0;
    ImVec2 size = POPARG(as_imvec2, ImVec2(0, 0));
    bool ret = ImGui::Selectable(label, selected, flags, size);
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(pushstylecolor)
{
    ImGuiCol idx = POPARG(as_imguicol, ImGuiCol_Text);
    ImU32 col = POPARG(as_uint, 0);
    ImGui::PushStyleColor(idx, col);
}
APIFUNC_END

APIFUNC(popstylecolor)
{
    int count = POPARG(as_int, 1);
    ImGui::PopStyleColor(count);
}
APIFUNC_END

// Bindings definition

static void define(const char *name, cl_objectfn fn)
{
    ecl_shadow(ecl_read_from_cstring(name), ecl_current_package());
    ecl_def_c_function_va(ecl_read_from_cstring(name), fn);
}

void cl_define_bindings()
{
    define("begin", clapi_begin);
    define("end", clapi_end);
    define("text", clapi_text);
    define("button", clapi_button);
    define("same-line", clapi_sameline);
    define("separator", clapi_separator);
    define("begin-group", clapi_begingroup);
    define("end-group", clapi_endgroup);
    define("push-item-width", clapi_pushitemwidth);
    define("pop-item-width", clapi_popitemwidth);
    define("collapsing-header", clapi_collapsingheader);
    define("spacing", clapi_spacing);
    define("bullet-text", clapi_bullettext);
    define("tree-node", clapi_treenode);
    define("tree-pop", clapi_treepop);
    define("set-next-item-width", clapi_setnextitemwidth);
    define("drag-float", clapi_dragfloat);
    define("push-style-var", clapi_pushstylevar);
    define("pop-style-var", clapi_popstylevar);
    define("columns", clapi_columns);
    define("push-id", clapi_pushid);
    define("pop-id", clapi_popid);
    define("align-text-to-frame-padding", clapi_aligntexttoframepadding);
    define("next-column", clapi_nextcolumn);
    define("input-float", clapi_inputfloat);
    define("set-next-window-size", clapi_setnextwindowsize);
    define("begin-child", clapi_beginchild);
    define("end-child", clapi_endchild);
    define("selectable", clapi_selectable);
    define("push-style-color", clapi_pushstylecolor);
    define("pop-style-color", clapi_popstylecolor);
}
