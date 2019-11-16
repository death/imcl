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

#define RETSTRING(x) result = ecl_make_simple_base_string((x), -1)

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

#define APIFUNC2(name)                                  \
    static cl_object clapi_ ## name(cl_narg nargs, ...) \
    {                                                   \
    ecl_va_list ap;                                     \
    cl_object result1 = ECL_NIL;                        \
    cl_object result2 = ECL_NIL;                        \
    ecl_va_start(ap, nargs, nargs, 0);

#define APIFUNC2_END                             \
    ecl_va_end(ap);                              \
    ecl_return2(ecl_process_env(), result1, result2);   \
    }

#define RETIMVEC2(v)                            \
    result1 = ecl_make_single_float((v).x);     \
    result2 = ecl_make_single_float((v).y);

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

int keyword_flags_value(cl_object flags, struct keyword_enum_descriptor *descriptors, int size)
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

ImVec4 uint_to_imvec4(unsigned int val)
{
    ImVec4 imvec;
    imvec.x = (val >> 0) & 0xFF;
    imvec.y = (val >> 8) & 0xFF;
    imvec.z = (val >> 16) & 0xFF;
    imvec.w = (val >> 24) & 0xFF;
    imvec.x /= 255.0;
    imvec.y /= 255.0;
    imvec.z /= 255.0;
    imvec.w /= 255.0;
    return imvec;
}

bool ecl_imvec4_p(cl_object object, ImVec4 & result)
{
    if (cl_integerp(object) != ECL_NIL) {
        unsigned int val = as_uint(object);
        result = uint_to_imvec4(val);
        return true;
    }
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
    if (cl_consp(cddr) == ECL_NIL) {
        return false;
    }
    cl_object caddr = cl_car(cddr);
    if (!ecl_realp(caddr)) {
        return false;
    }
    cl_object cdddr = cl_cdr(cddr);
    if (cl_consp(cdddr) == ECL_NIL) {
        return false;
    }
    cl_object cadddr = cl_car(cdddr);
    if (!ecl_realp(cadddr)) {
        return false;
    }
    cl_object cddddr = cl_cdr(cdddr);
    if (cl_null(cddddr) == ECL_NIL) {
        return false;
    }
    result.x = ecl_to_float(car);
    result.y = ecl_to_float(cadr);
    result.z = ecl_to_float(caddr);
    result.w = ecl_to_float(cadddr);
    return true;
}

ImVec4 as_imvec4(cl_object obj)
{
    ImVec4 imvec(0, 0, 0, 0);
    ecl_imvec4_p(obj, imvec);
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

struct keyword_enum_descriptor imguiwindowflags[] = {
    {"NONE", ImGuiWindowFlags_None},
    {"NO-TITLE-BAR", ImGuiWindowFlags_NoTitleBar},
    {"NO-RESIZE", ImGuiWindowFlags_NoResize},
    {"NO-MOVE", ImGuiWindowFlags_NoMove},
    {"NO-SCROLLBAR", ImGuiWindowFlags_NoScrollbar},
    {"NO-SCROLL-WITH-MOUSE", ImGuiWindowFlags_NoScrollWithMouse},
    {"NO-COLLAPSE", ImGuiWindowFlags_NoCollapse},
    {"ALWAYS-AUTO-RESIZE", ImGuiWindowFlags_AlwaysAutoResize},
    {"NO-BACKGROUND", ImGuiWindowFlags_NoBackground},
    {"NO-SAVED-SETTINGS", ImGuiWindowFlags_NoSavedSettings},
    {"NO-MOUSE-INPUTS", ImGuiWindowFlags_NoMouseInputs},
    {"MENU-BAR", ImGuiWindowFlags_MenuBar},
    {"HORIZONTAL-SCROLLBAR", ImGuiWindowFlags_HorizontalScrollbar},
    {"NO-FOCUS-ON-APPEARING", ImGuiWindowFlags_NoFocusOnAppearing},
    {"NO-BRING-TO-FRONT-ON-FOCUS", ImGuiWindowFlags_NoBringToFrontOnFocus},
    {"ALWAYS-VERTICAL-SCROLLBAR", ImGuiWindowFlags_AlwaysVerticalScrollbar},
    {"ALWAYS-HORIZONTAL-SCROLLBAR", ImGuiWindowFlags_AlwaysHorizontalScrollbar},
    {"ALWAYS-USE-WINDOW-PADDING", ImGuiWindowFlags_AlwaysUseWindowPadding},
    {"NO-NAV-INPUTS", ImGuiWindowFlags_NoNavInputs},
    {"NO-NAV-FOCUS", ImGuiWindowFlags_NoNavFocus},
    {"UNSAVED-DOCUMENT", ImGuiWindowFlags_UnsavedDocument},
    {"NO-NAV", ImGuiWindowFlags_NoNav},
    {"NO-DECORATION", ImGuiWindowFlags_NoDecoration},
    {"NO-INPUTS", ImGuiWindowFlags_NoInputs},
};

ImGuiWindowFlags as_imguiwindowflags(cl_object obj)
{
    ImGuiWindowFlags flags = ImGuiWindowFlags_None;
    if (obj != ECL_NIL) {
        flags = keyword_flags_value(obj, imguiwindowflags, LENGTHOF(imguiwindowflags));
    }
    return flags;
}

struct keyword_enum_descriptor imguifocusedflags[] = {
    {"NONE", ImGuiFocusedFlags_None},
    {"CHILD-WINDOWS", ImGuiFocusedFlags_ChildWindows},
    {"ROOT-WINDOW", ImGuiFocusedFlags_RootWindow},
    {"ANY-WINDOW", ImGuiFocusedFlags_AnyWindow},
    {"ROOT-AND-CHILD-WINDOWS", ImGuiFocusedFlags_RootAndChildWindows},
};

ImGuiFocusedFlags as_imguifocusedflags(cl_object obj)
{
    ImGuiFocusedFlags flags = ImGuiFocusedFlags_None;
    if (obj != ECL_NIL) {
        flags = keyword_flags_value(obj, imguifocusedflags, LENGTHOF(imguifocusedflags));
    }
    return flags;
}

struct keyword_enum_descriptor imguihoveredflags[] = {
    {"NONE", ImGuiHoveredFlags_None},
    {"CHILD-WINDOWS", ImGuiHoveredFlags_ChildWindows},
    {"ROOT-WINDOW", ImGuiHoveredFlags_RootWindow},
    {"ANY-WINDOW", ImGuiHoveredFlags_AnyWindow},
    {"ALLOW-WHEN-BLOCKED-BY-POPUP", ImGuiHoveredFlags_AllowWhenBlockedByPopup},
    {"ALLOW-WHEN-BLOCKED-BY-ACTIVE-ITEM", ImGuiHoveredFlags_AllowWhenBlockedByActiveItem},
    {"ALLOW-WHEN-OVERLAPPED", ImGuiHoveredFlags_AllowWhenOverlapped},
    {"ALLOW-WHEN-DISABLED", ImGuiHoveredFlags_AllowWhenDisabled},
    {"RECT-ONLY", ImGuiHoveredFlags_RectOnly},
    {"ROOT-AND-CHILD-WINDOWS", ImGuiHoveredFlags_RootAndChildWindows},
};

ImGuiHoveredFlags as_imguihoveredflags(cl_object obj)
{
    ImGuiHoveredFlags flags = ImGuiHoveredFlags_None;
    if (obj != ECL_NIL) {
        flags = keyword_flags_value(obj, imguihoveredflags, LENGTHOF(imguihoveredflags));
    }
    return flags;
}

struct keyword_enum_descriptor imguidir[] = {
    {"NONE", ImGuiDir_None},
    {"LEFT", ImGuiDir_Left},
    {"RIGHT", ImGuiDir_Right},
    {"UP", ImGuiDir_Up},
    {"DOWN", ImGuiDir_Down},
};

ImGuiDir as_imguidir(cl_object obj)
{
    ImGuiDir dir = ImGuiDir_None;
    if (obj != ECL_NIL) {
        dir = keyword_enum_value(obj, imguidir, LENGTHOF(imguidir));
    }
    return dir;
}

struct keyword_enum_descriptor imguiselectableflags[] = {
    {"NONE", ImGuiSelectableFlags_None},
    {"DONT-CLOSE-POPUPS", ImGuiSelectableFlags_DontClosePopups},
    {"SPAN-ALL-COLUMNS", ImGuiSelectableFlags_SpanAllColumns},
    {"ALLOW-DOUBLE-CLICK", ImGuiSelectableFlags_AllowDoubleClick},
    {"DISABLED", ImGuiSelectableFlags_Disabled},
};

ImGuiSelectableFlags as_imguiselectableflags(cl_object obj)
{
    ImGuiSelectableFlags flags = ImGuiSelectableFlags_None;
    if (obj != ECL_NIL) {
        flags = keyword_flags_value(obj, imguiselectableflags, LENGTHOF(imguiselectableflags));
    }
    return flags;
}

struct keyword_enum_descriptor imguiinputtextflags[] = {
    {"NONE", ImGuiInputTextFlags_None},
    {"CHARS-DECIMAL", ImGuiInputTextFlags_CharsDecimal},
    {"CHARS-HEXADECIMAL", ImGuiInputTextFlags_CharsHexadecimal},
    {"CHARS-UPPERCASE", ImGuiInputTextFlags_CharsUppercase},
    {"CHARS-NO-BLANK", ImGuiInputTextFlags_CharsNoBlank},
    {"AUTO-SELECT-ALL", ImGuiInputTextFlags_AutoSelectAll},
    {"ENTER-RETURNS-TRUE", ImGuiInputTextFlags_EnterReturnsTrue},
    {"CALLBACK-COMPLETION", ImGuiInputTextFlags_CallbackCompletion},
    {"CALLBACK-HISTORY", ImGuiInputTextFlags_CallbackHistory},
    {"CALLBACK-ALWAYS", ImGuiInputTextFlags_CallbackAlways},
    {"CALLBACK-CHAR-FILTER", ImGuiInputTextFlags_CallbackCharFilter},
    {"ALLOW-TAB-INPUT", ImGuiInputTextFlags_AllowTabInput},
    {"CTRL-ENTER-FOR-NEW-LINE", ImGuiInputTextFlags_CtrlEnterForNewLine},
    {"NO-HORIZONTAL-SCROLL", ImGuiInputTextFlags_NoHorizontalScroll},
    {"ALWAYS-INSERT-MODE", ImGuiInputTextFlags_AlwaysInsertMode},
    {"READ-ONLY", ImGuiInputTextFlags_ReadOnly},
    {"PASSWORD", ImGuiInputTextFlags_Password},
    {"NO-UNDO-REDO", ImGuiInputTextFlags_NoUndoRedo},
    {"CHARS-SCIENTIFIC", ImGuiInputTextFlags_CharsScientific},
    {"CALLBACK-RESIZE", ImGuiInputTextFlags_CallbackResize},
};

ImGuiInputTextFlags as_imguiinputtextflags(cl_object obj)
{
    ImGuiInputTextFlags flags = ImGuiInputTextFlags_None;
    if (obj != ECL_NIL) {
        flags = keyword_flags_value(obj, imguiinputtextflags, LENGTHOF(imguiinputtextflags));
    }
    return flags;
}

// The actual bindings

APIFUNC(begin)
{
    const char *label = POPARG(as_text, "Window");
    ImGuiWindowFlags flags = POPARG(as_imguiwindowflags, ImGuiWindowFlags_None);
    bool ret = ImGui::Begin(label, NULL, flags);
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
    ImGui::TextUnformatted(label);
}
APIFUNC_END

APIFUNC(textcolored)
{
    ImVec4 color = POPARG(as_imvec4, ImVec4(0, 0, 0, 0));
    const char *text = POPARG(as_text, "Text");
    ImGui::TextColored(color, "%s", text);
}
APIFUNC_END

APIFUNC(textdisabled)
{
    const char *text = POPARG(as_text, "Text");
    ImGui::TextDisabled("%s", text);
}
APIFUNC_END

APIFUNC(textwrapped)
{
    const char *text = POPARG(as_text, "Text");
    ImGui::TextWrapped("%s", text);
}
APIFUNC_END

APIFUNC(labeltext)
{
    const char *label = POPARG(as_text, "label");
    const char *text = POPARG(as_text, "text");
    ImGui::LabelText(label, "%s", text);
}
APIFUNC_END


APIFUNC(bullettext)
{
    const char *label = POPARG(as_text, "BulletText");
    ImGui::BulletText("%s", label);
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
    float offset = POPARG(as_float, 0.0F);
    float spacing = POPARG(as_float, -1.0F);
    ImGui::SameLine(offset, spacing);
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

APIFUNC(setnextwindowsize)
{
    ImVec2 size = POPARG(as_imvec2, ImVec2(0, 0));
    ImGuiCond cond = POPARG(as_imguicond, ImGuiCond_Always);
    ImGui::SetNextWindowSize(size, cond);
}
APIFUNC_END

APIFUNC(beginchild)
{
    // TODO: ImGuiID variant?
    const char *id = POPARG(as_text, "child");
    ImVec2 size = POPARG(as_imvec2, ImVec2(0, 0));
    bool border = POPARG(as_bool, false);
    ImGuiWindowFlags flags = POPARG(as_imguiwindowflags, ImGuiWindowFlags_None);
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
    ImGuiSelectableFlags flags = POPARG(as_imguiselectableflags, ImGuiSelectableFlags_None);
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

APIFUNC(showdemowindow)
{
    bool show = true;
    ImGui::ShowDemoWindow(&show);
    RETBOOL(show);
}
APIFUNC_END

APIFUNC2(checkbox)
{
    const char *label = POPARG(as_text, "checkbox");
    bool v = POPARG(as_bool, false);
    bool ret = ImGui::Checkbox(label, &v);
    result1 = v ? ECL_T : ECL_NIL;
    result2 = ret ? ECL_T : ECL_NIL;
}
APIFUNC2_END

APIFUNC(radiobutton)
{
    const char *label = POPARG(as_text, "radio");
    bool active = POPARG(as_bool, false);
    bool pressed = ImGui::RadioButton(label, active);
    RETBOOL(pressed);
}
APIFUNC_END

APIFUNC(isitemhovered)
{
    ImGuiHoveredFlags flags = POPARG(as_imguihoveredflags, ImGuiHoveredFlags_None);
    bool hovered = ImGui::IsItemHovered(flags);
    RETBOOL(hovered);
}
APIFUNC_END

APIFUNC(settooltip)
{
    const char *tip = POPARG(as_text, "tooltip");
    ImGui::SetTooltip("%s", tip);
}
APIFUNC_END

APIFUNC(begintooltip)
{
    ImGui::BeginTooltip();
}
APIFUNC_END

APIFUNC(endtooltip)
{
    ImGui::EndTooltip();
}
APIFUNC_END

APIFUNC(getclipboardtext)
{
    const char *text = ImGui::GetClipboardText();
    RETSTRING(text);
}
APIFUNC_END

APIFUNC(setclipboardtext)
{
    const char *text = POPARG(as_text, "text");
    ImGui::SetClipboardText(text);
}
APIFUNC_END

APIFUNC(begintabbar)
{
    const char *id = POPARG(as_text, "tabid");
    // TODO: flags arg
    ImGuiTabBarFlags flags = 0;
    bool ret = ImGui::BeginTabBar(id, flags);
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(endtabbar)
{
    ImGui::EndTabBar();
}
APIFUNC_END

APIFUNC(begintabitem)
{
    const char *label = POPARG(as_text, "tabitem");
    // TODO: p_open arg
    // TODO: flags arg
    ImGuiTabItemFlags flags = 0;
    bool ret = ImGui::BeginTabItem(label, NULL, flags);
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(endtabitem)
{
    ImGui::EndTabItem();
}
APIFUNC_END

APIFUNC(settabitemclosed)
{
    const char *label = POPARG(as_text, "label");
    ImGui::SetTabItemClosed(label);
}
APIFUNC_END

APIFUNC(setitemdefaultfocus)
{
    ImGui::SetItemDefaultFocus();
}
APIFUNC_END

APIFUNC(setkeyboardfocushere)
{
    int offset = POPARG(as_int, 0);
    ImGui::SetKeyboardFocusHere(offset);
}
APIFUNC_END

APIFUNC(stylecolors)
{
    cl_object dark = ecl_make_keyword("DARK");
    cl_object classic = ecl_make_keyword("CLASSIC");
    cl_object light = ecl_make_keyword("LIGHT");
    cl_object which = POPARG(as_object, dark);
    bool ret = true;
    if (ecl_equal(which, dark)) {
        ImGui::StyleColorsDark();
    } else if (ecl_equal(which, classic)) {
        ImGui::StyleColorsClassic();
    } else if (ecl_equal(which, light)) {
        ImGui::StyleColorsLight();
    } else {
        ret = false;
    }
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(begincombo)
{
    const char *label = POPARG(as_text, "label");
    const char *preview_value = POPARG(as_text, "preview");
    // TODO: flags
    ImGuiComboFlags flags = 0;
    bool ret = ImGui::BeginCombo(label, preview_value, flags);
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(endcombo)
{
    ImGui::EndCombo();
}
APIFUNC_END

APIFUNC(beginlistbox)
{
    // TODO: overload taking ImVec2
    const char *label = POPARG(as_text, "label");
    int nitems = POPARG(as_int, 0);
    int height = POPARG(as_int, -1);
    bool ret = ImGui::ListBoxHeader(label, nitems, height);
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(endlistbox)
{
    ImGui::ListBoxFooter();
}
APIFUNC_END

APIFUNC(beginmainmenubar)
{
    bool ret = ImGui::BeginMainMenuBar();
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(endmainmenubar)
{
    ImGui::EndMainMenuBar();
}
APIFUNC_END

APIFUNC(beginmenubar)
{
    bool ret = ImGui::BeginMenuBar();
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(endmenubar)
{
    ImGui::EndMenuBar();
}
APIFUNC_END

APIFUNC(beginmenu)
{
    const char *label = POPARG(as_text, "label");
    bool enabled = POPARG(as_bool, true);
    bool ret = ImGui::BeginMenu(label, enabled);
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(endmenu)
{
    ImGui::EndMenu();
}
APIFUNC_END

APIFUNC(menuitem)
{
    const char *label = POPARG(as_text, "label");
    const char *shortcut = POPARG(as_text, NULL);
    bool selected = POPARG(as_bool, false);
    bool enabled = POPARG(as_bool, true);
    bool ret = ImGui::MenuItem(label, shortcut, selected, enabled);
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(getscrollx)
{
    float ret = ImGui::GetScrollX();
    RETFLOAT(ret);
}
APIFUNC_END

APIFUNC(getscrolly)
{
    float ret = ImGui::GetScrollY();
    RETFLOAT(ret);
}
APIFUNC_END

APIFUNC(getscrollmaxx)
{
    float ret = ImGui::GetScrollMaxX();
    RETFLOAT(ret);
}
APIFUNC_END

APIFUNC(getscrollmaxy)
{
    float ret = ImGui::GetScrollMaxY();
    RETFLOAT(ret);
}
APIFUNC_END

APIFUNC(setscrollx)
{
    float x = POPARG(as_float, 0.0F);
    ImGui::SetScrollX(x);
}
APIFUNC_END

APIFUNC(setscrolly)
{
    float y = POPARG(as_float, 0.0F);
    ImGui::SetScrollY(y);
}
APIFUNC_END

APIFUNC(setscrollherey)
{
    float r = POPARG(as_float, 0.5F);
    ImGui::SetScrollHereY(r);
}
APIFUNC_END

APIFUNC(setscrollfromposy)
{
    float y = POPARG(as_float, 0.0F);
    float r = POPARG(as_float, 0.5F);
    ImGui::SetScrollFromPosY(y, r);
}
APIFUNC_END

APIFUNC(sliderfloat)
{
    const char *label = POPARG(as_text, "label");
    cl_object val = POPARG(as_object, ECL_NIL);
    float v_min = POPARG(as_float, 0.0F);
    float v_max = POPARG(as_float, 1.0F);
    const char *format = POPARG(as_text, "%.3f");
    float power = POPARG(as_float, 1.0F);
    bool ret = false;
    float v[4];
    int n = 0;
    cl_object sub = val;
    while (cl_consp(sub) != ECL_NIL && n < 4) {
        cl_object car = cl_car(sub);
        if (!ecl_realp(car)) {
            n = 0;
            break;
        }
        v[n] = as_float(car);
        n++;
        sub = cl_cdr(sub);
    }
    switch (n) {
    case 0:
        break;
    case 1:
        ret = ImGui::SliderFloat(label, v, v_min, v_max, format, power);
        break;
    case 2:
        ret = ImGui::SliderFloat2(label, v, v_min, v_max, format, power);
        break;
    case 3:
        ret = ImGui::SliderFloat3(label, v, v_min, v_max, format, power);
        break;
    case 4:
        ret = ImGui::SliderFloat4(label, v, v_min, v_max, format, power);
        break;
    default:
        break;
    }
    sub = val;
    for (int i = 0; i < n; i++) {
        cl_object f = ecl_make_single_float(v[i]);
        cl_rplaca(sub, f);
        sub = cl_cdr(sub);
    }
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(sliderangle)
{
    const char *label = POPARG(as_text, "label");
    cl_object val = POPARG(as_object, ECL_NIL);
    float v_degrees_min = POPARG(as_float, -360.0F);
    float v_degrees_max = POPARG(as_float, +360.0F);
    const char *format = POPARG(as_text, "%.0f deg");
    bool ret = false;
    if (cl_consp(val) != ECL_NIL) {
        cl_object car = cl_car(val);
        if (ecl_realp(car)) {
            float v_rad = as_float(car);
            ret = ImGui::SliderAngle(label, &v_rad, v_degrees_min, v_degrees_max, format);
            cl_rplaca(val, ecl_make_single_float(v_rad));
        }
    }
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(sliderint)
{
    const char *label = POPARG(as_text, "label");
    cl_object val = POPARG(as_object, ECL_NIL);
    int v_min = POPARG(as_int, 0);
    int v_max = POPARG(as_int, 100);
    const char *format = POPARG(as_text, "%d");
    bool ret = false;
    int v[4];
    int n = 0;
    cl_object sub = val;
    while (cl_consp(sub) != ECL_NIL && n < 4) {
        cl_object car = cl_car(sub);
        if (!ecl_realp(car)) {
            n = 0;
            break;
        }
        v[n] = as_int(car);
        n++;
        sub = cl_cdr(sub);
    }
    switch (n) {
    case 0:
        break;
    case 1:
        ret = ImGui::SliderInt(label, v, v_min, v_max, format);
        break;
    case 2:
        ret = ImGui::SliderInt2(label, v, v_min, v_max, format);
        break;
    case 3:
        ret = ImGui::SliderInt3(label, v, v_min, v_max, format);
        break;
    case 4:
        ret = ImGui::SliderInt4(label, v, v_min, v_max, format);
        break;
    default:
        break;
    }
    sub = val;
    for (int i = 0; i < n; i++) {
        cl_object f = ecl_make_int32_t(v[i]);
        cl_rplaca(sub, f);
        sub = cl_cdr(sub);
    }
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(vsliderfloat)
{
    const char *label = POPARG(as_text, "label");
    ImVec2 size = POPARG(as_imvec2, ImVec2(50, 50));
    cl_object val = POPARG(as_object, ECL_NIL);
    float v_min = POPARG(as_float, 0.0F);
    float v_max = POPARG(as_float, 1.0F);
    const char *format = POPARG(as_text, "%.3f");
    float power = POPARG(as_float, 1.0F);
    bool ret = false;
    if (cl_consp(val) != ECL_NIL) {
        cl_object car = cl_car(val);
        if (ecl_realp(car)) {
            float f = as_float(car);
            ret = ImGui::VSliderFloat(label, size, &f, v_min, v_max, format, power);
            cl_rplaca(val, ecl_make_single_float(f));
        }
    }
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(vsliderint)
{
    const char *label = POPARG(as_text, "label");
    ImVec2 size = POPARG(as_imvec2, ImVec2(50, 50));
    cl_object val = POPARG(as_object, ECL_NIL);
    int v_min = POPARG(as_int, 0);
    int v_max = POPARG(as_int, 100);
    const char *format = POPARG(as_text, "%d");
    bool ret = false;
    if (cl_consp(val) != ECL_NIL) {
        cl_object car = cl_car(val);
        if (ecl_realp(car)) {
            int v = as_int(car);
            ret = ImGui::VSliderInt(label, size, &v, v_min, v_max, format);
            cl_rplaca(val, ecl_make_int32_t(v));
        }
    }
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(iswindowappearing)
{
    bool ret = ImGui::IsWindowAppearing();
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(iswindowcollapsed)
{
    bool ret = ImGui::IsWindowCollapsed();
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(iswindowfocused)
{
    ImGuiFocusedFlags flags = POPARG(as_imguifocusedflags, ImGuiFocusedFlags_None);
    bool ret = ImGui::IsWindowFocused(flags);
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(iswindowhovered)
{
    ImGuiHoveredFlags flags = POPARG(as_imguihoveredflags, ImGuiHoveredFlags_None);
    bool ret = ImGui::IsWindowHovered(flags);
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC2(getwindowpos)
{
    ImVec2 pos = ImGui::GetWindowPos();
    RETIMVEC2(pos);
}
APIFUNC2_END

APIFUNC2(getwindowsize)
{
    ImVec2 size = ImGui::GetWindowSize();
    RETIMVEC2(size);
}
APIFUNC2_END

APIFUNC(setnextwindowpos)
{
    ImVec2 pos = POPARG(as_imvec2, ImVec2(0, 0));
    ImGuiCond cond = POPARG(as_imguicond, ImGuiCond_Always);
    ImVec2 pivot = POPARG(as_imvec2, ImVec2(0, 0));
    ImGui::SetNextWindowPos(pos, cond, pivot);
}
APIFUNC_END

APIFUNC(setnextwindowcontentsize)
{
    ImVec2 size = POPARG(as_imvec2, ImVec2(0, 0));
    ImGui::SetNextWindowContentSize(size);
}
APIFUNC_END

APIFUNC(setnextwindowcollapsed)
{
    bool collapsed = POPARG(as_bool, true);
    ImGuiCond cond = POPARG(as_imguicond, ImGuiCond_Always);
    ImGui::SetNextWindowCollapsed(collapsed, cond);
}
APIFUNC_END

APIFUNC(setnextwindowfocus)
{
    ImGui::SetNextWindowFocus();
}
APIFUNC_END

APIFUNC(setnextwindowbgalpha)
{
    float alpha = POPARG(as_float, 1.0);
    ImGui::SetNextWindowBgAlpha(alpha);
}
APIFUNC_END

APIFUNC(setwindowpos)
{
    cl_object name_or_pos = POPARG(as_object, ECL_NIL);
    const char *name = 0;
    ImVec2 pos;
    if (ecl_stringp(name_or_pos)) {
        name = as_text(name_or_pos);
        pos = POPARG(as_imvec2, ImVec2(0, 0));
    } else {
        pos = as_imvec2(name_or_pos);
    }
    ImGuiCond cond = POPARG(as_imguicond, ImGuiCond_Always);
    if (name) {
        ImGui::SetWindowPos(name, pos, cond);
    } else {
        ImGui::SetWindowPos(pos, cond);
    }
}
APIFUNC_END

APIFUNC(setwindowsize)
{
    cl_object name_or_size = POPARG(as_object, ECL_NIL);
    const char *name = 0;
    ImVec2 size;
    if (ecl_stringp(name_or_size)) {
        name = as_text(name_or_size);
        size = POPARG(as_imvec2, ImVec2(0, 0));
    } else {
        size = as_imvec2(name_or_size);
    }
    ImGuiCond cond = POPARG(as_imguicond, ImGuiCond_Always);
    if (name) {
        ImGui::SetWindowSize(name, size, cond);
    } else {
        ImGui::SetWindowSize(size, cond);
    }
}
APIFUNC_END

APIFUNC(setwindowcollapsed)
{
    cl_object name_or_collapsed = POPARG(as_object, ECL_NIL);
    const char *name = 0;
    bool collapsed = true;
    if (ecl_stringp(name_or_collapsed)) {
        name = as_text(name_or_collapsed);
        collapsed = POPARG(as_bool, true);
    } else {
        collapsed = as_bool(name_or_collapsed);
    }
    ImGuiCond cond = POPARG(as_imguicond, ImGuiCond_Always);
    if (name) {
        ImGui::SetWindowCollapsed(name, collapsed, cond);
    } else {
        ImGui::SetWindowCollapsed(collapsed, cond);
    }
}
APIFUNC_END

APIFUNC(setwindowfocus)
{
    const char *name = POPARG(as_text, 0);
    if (name) {
        ImGui::SetWindowFocus(name);
    } else {
        ImGui::SetWindowFocus();
    }
}
APIFUNC_END

APIFUNC(setwindowfontscale)
{
    float scale = POPARG(as_float, 1.0);
    ImGui::SetWindowFontScale(scale);
}
APIFUNC_END

APIFUNC(calcitemwidth)
{
    float width = ImGui::CalcItemWidth();
    RETFLOAT(width);
}
APIFUNC_END

APIFUNC(pushtextwrappos)
{
    float pos = POPARG(as_float, 0.0F);
    ImGui::PushTextWrapPos(pos);
}
APIFUNC_END

APIFUNC(poptextwrappos)
{
    ImGui::PopTextWrapPos();
}
APIFUNC_END

APIFUNC(pushallowkeyboardfocus)
{
    bool allow = POPARG(as_bool, true);
    ImGui::PushAllowKeyboardFocus(allow);
}
APIFUNC_END

APIFUNC(popallowkeyboardfocus)
{
    ImGui::PopAllowKeyboardFocus();
}
APIFUNC_END

APIFUNC(pushbuttonrepeat)
{
    bool repeat = POPARG(as_bool, true);
    ImGui::PushButtonRepeat(repeat);
}
APIFUNC_END

APIFUNC(popbuttonrepeat)
{
    ImGui::PopButtonRepeat();
}
APIFUNC_END

APIFUNC(newline)
{
    ImGui::NewLine();
}
APIFUNC_END

APIFUNC(dummy)
{
    ImVec2 size = POPARG(as_imvec2, ImVec2(0, 0));
    ImGui::Dummy(size);
}
APIFUNC_END

APIFUNC(indent)
{
    float w = POPARG(as_float, 0.0F);
    ImGui::Indent(w);
}
APIFUNC_END

APIFUNC(unindent)
{
    float w = POPARG(as_float, 0.0F);
    ImGui::Unindent(w);
}
APIFUNC_END

APIFUNC2(getcursorpos)
{
    ImVec2 pos = ImGui::GetCursorPos();
    RETIMVEC2(pos);
}
APIFUNC2_END

APIFUNC(setcursorpos)
{
    ImVec2 pos = POPARG(as_imvec2, ImVec2(0, 0));
    ImGui::SetCursorPos(pos);
}
APIFUNC_END

APIFUNC2(getcursorstartpos)
{
    ImVec2 pos = ImGui::GetCursorStartPos();
    RETIMVEC2(pos);
}
APIFUNC2_END

APIFUNC2(getcursorscreenpos)
{
    ImVec2 pos = ImGui::GetCursorScreenPos();
    RETIMVEC2(pos);
}
APIFUNC2_END

APIFUNC(setcursorscreenpos)
{
    ImVec2 pos = POPARG(as_imvec2, ImVec2(0, 0));
    ImGui::SetCursorScreenPos(pos);
}
APIFUNC_END

APIFUNC(gettextlineheight)
{
    float height = ImGui::GetTextLineHeight();
    RETFLOAT(height);
}
APIFUNC_END

APIFUNC(gettextlineheightwithspacing)
{
    float height = ImGui::GetTextLineHeightWithSpacing();
    RETFLOAT(height);
}
APIFUNC_END

APIFUNC(getframeheight)
{
    float height = ImGui::GetFrameHeight();
    RETFLOAT(height);
}
APIFUNC_END

APIFUNC(getframeheightwithspacing)
{
    float height = ImGui::GetFrameHeightWithSpacing();
    RETFLOAT(height);
}
APIFUNC_END

APIFUNC(smallbutton)
{
    const char *label = POPARG(as_text, "label");
    bool ret = ImGui::SmallButton(label);
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(invisiblebutton)
{
    const char *id = POPARG(as_text, "id");
    ImVec2 size = POPARG(as_imvec2, ImVec2(1, 1));
    bool ret = ImGui::InvisibleButton(id, size);
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(arrowbutton)
{
    const char *id = POPARG(as_text, "id");
    ImGuiDir dir = POPARG(as_imguidir, ImGuiDir_Left);
    bool ret = ImGui::ArrowButton(id, dir);
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(bullet)
{
    ImGui::Bullet();
}
APIFUNC_END

APIFUNC2(getcontentregionmax)
{
    ImVec2 ret = ImGui::GetContentRegionMax();
    RETIMVEC2(ret);
}
APIFUNC2_END

APIFUNC2(getcontentregionavail)
{
    ImVec2 ret = ImGui::GetContentRegionAvail();
    RETIMVEC2(ret);
}
APIFUNC2_END

APIFUNC2(getwindowcontentregionmin)
{
    ImVec2 ret = ImGui::GetWindowContentRegionMin();
    RETIMVEC2(ret);
}
APIFUNC2_END

APIFUNC2(getwindowcontentregionmax)
{
    ImVec2 ret = ImGui::GetWindowContentRegionMax();
    RETIMVEC2(ret);
}
APIFUNC2_END

APIFUNC(progressbar)
{
    float fraction = POPARG(as_float, 0.0F);
    ImVec2 size = POPARG(as_imvec2, ImVec2(-1, 0));
    const char *overlay = POPARG(as_text, NULL);
    ImGui::ProgressBar(fraction, size, overlay);
}
APIFUNC_END

APIFUNC(isitemactive)
{
    bool ret = ImGui::IsItemActive();
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(isitemfocused)
{
    bool ret = ImGui::IsItemFocused();
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(isitemclicked)
{
    int button = POPARG(as_int, 0);
    bool ret = ImGui::IsItemClicked(button);
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(isitemvisible)
{
    bool ret = ImGui::IsItemVisible();
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(isitemedited)
{
    bool ret = ImGui::IsItemEdited();
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(isitemactivated)
{
    bool ret = ImGui::IsItemActivated();
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(isitemdeactivated)
{
    bool ret = ImGui::IsItemDeactivated();
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(isitemdeactivatedafteredit)
{
    bool ret = ImGui::IsItemDeactivatedAfterEdit();
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(isanyitemhovered)
{
    bool ret = ImGui::IsAnyItemHovered();
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(isanyitemactive)
{
    bool ret = ImGui::IsAnyItemActive();
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(isanyitemfocused)
{
    bool ret = ImGui::IsAnyItemFocused();
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(dragfloat)
{
    const char *label = POPARG(as_text, "label");
    cl_object val = POPARG(as_object, ECL_NIL);
    float v_speed = POPARG(as_float, 1.0F);
    float v_min = POPARG(as_float, 0.0F);
    float v_max = POPARG(as_float, 0.0F);
    const char *format = POPARG(as_text, "%.3f");
    float power = POPARG(as_float, 1.0F);
    bool ret = false;
    float v[4];
    int n = 0;
    cl_object sub = val;
    while (cl_consp(sub) != ECL_NIL && n < 4) {
        cl_object car = cl_car(sub);
        if (!ecl_realp(car)) {
            n = 0;
            break;
        }
        v[n] = as_float(car);
        n++;
        sub = cl_cdr(sub);
    }
    switch (n) {
    case 0:
        break;
    case 1:
        ret = ImGui::DragFloat(label, v, v_speed, v_min, v_max, format, power);
        break;
    case 2:
        ret = ImGui::DragFloat2(label, v, v_speed, v_min, v_max, format, power);
        break;
    case 3:
        ret = ImGui::DragFloat3(label, v, v_speed, v_min, v_max, format, power);
        break;
    case 4:
        ret = ImGui::DragFloat4(label, v, v_speed, v_min, v_max, format, power);
        break;
    default:
        break;
    }
    sub = val;
    for (int i = 0; i < n; i++) {
        cl_object f = ecl_make_single_float(v[i]);
        cl_rplaca(sub, f);
        sub = cl_cdr(sub);
    }
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(inputfloat)
{
    const char *label = POPARG(as_text, "label");
    cl_object val = POPARG(as_object, ECL_NIL);
    float step = POPARG(as_float, 0.0F);
    float step_fast = POPARG(as_float, 0.0F);
    const char *format = POPARG(as_text, "%.3f");
    ImGuiInputTextFlags flags = POPARG(as_imguiinputtextflags, ImGuiInputTextFlags_None);
    bool ret = false;
    float v[4];
    int n = 0;
    cl_object sub = val;
    while (cl_consp(sub) != ECL_NIL && n < 4) {
        cl_object car = cl_car(sub);
        if (!ecl_realp(car)) {
            n = 0;
            break;
        }
        v[n] = as_float(car);
        n++;
        sub = cl_cdr(sub);
    }
    switch (n) {
    case 0:
        break;
    case 1:
        ret = ImGui::InputFloat(label, v, step, step_fast, format, flags);
        break;
    case 2:
        ret = ImGui::InputFloat2(label, v, format, flags);
        break;
    case 3:
        ret = ImGui::InputFloat3(label, v, format, flags);
        break;
    case 4:
        ret = ImGui::InputFloat4(label, v, format, flags);
        break;
    default:
        break;
    }
    sub = val;
    for (int i = 0; i < n; i++) {
        cl_object f = ecl_make_single_float(v[i]);
        cl_rplaca(sub, f);
        sub = cl_cdr(sub);
    }
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(dragint)
{
    const char *label = POPARG(as_text, "label");
    cl_object val = POPARG(as_object, ECL_NIL);
    float v_speed = POPARG(as_float, 1.0F);
    int v_min = POPARG(as_int, 0);
    int v_max = POPARG(as_int, 0);
    const char *format = POPARG(as_text, "%d");
    bool ret = false;
    int v[4];
    int n = 0;
    cl_object sub = val;
    while (cl_consp(sub) != ECL_NIL && n < 4) {
        cl_object car = cl_car(sub);
        if (!ecl_realp(car)) {
            n = 0;
            break;
        }
        v[n] = as_int(car);
        n++;
        sub = cl_cdr(sub);
    }
    switch (n) {
    case 0:
        break;
    case 1:
        ret = ImGui::DragInt(label, v, v_speed, v_min, v_max, format);
        break;
    case 2:
        ret = ImGui::DragInt2(label, v, v_speed, v_min, v_max, format);
        break;
    case 3:
        ret = ImGui::DragInt3(label, v, v_speed, v_min, v_max, format);
        break;
    case 4:
        ret = ImGui::DragInt4(label, v, v_speed, v_min, v_max, format);
        break;
    default:
        break;
    }
    sub = val;
    for (int i = 0; i < n; i++) {
        cl_object f = ecl_make_int32_t(v[i]);
        cl_rplaca(sub, f);
        sub = cl_cdr(sub);
    }
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(inputint)
{
    const char *label = POPARG(as_text, "label");
    cl_object val = POPARG(as_object, ECL_NIL);
    int step = POPARG(as_int, 1);
    int step_fast = POPARG(as_int, 100);
    ImGuiInputTextFlags flags = POPARG(as_imguiinputtextflags, ImGuiInputTextFlags_None);
    bool ret = false;
    int v[4];
    int n = 0;
    cl_object sub = val;
    while (cl_consp(sub) != ECL_NIL && n < 4) {
        cl_object car = cl_car(sub);
        if (!ecl_realp(car)) {
            n = 0;
            break;
        }
        v[n] = as_int(car);
        n++;
        sub = cl_cdr(sub);
    }
    switch (n) {
    case 0:
        break;
    case 1:
        ret = ImGui::InputInt(label, v, step, step_fast, flags);
        break;
    case 2:
        ret = ImGui::InputInt2(label, v, flags);
        break;
    case 3:
        ret = ImGui::InputInt3(label, v, flags);
        break;
    case 4:
        ret = ImGui::InputInt4(label, v, flags);
        break;
    default:
        break;
    }
    sub = val;
    for (int i = 0; i < n; i++) {
        cl_object f = ecl_make_int32_t(v[i]);
        cl_rplaca(sub, f);
        sub = cl_cdr(sub);
    }
    RETBOOL(ret);
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
    define("%text-colored", clapi_textcolored);
    define("text-disabled", clapi_textdisabled);
    define("text-wrapped", clapi_textwrapped);
    define("label-text", clapi_labeltext);
    define("bullet-text", clapi_bullettext);
    define("button", clapi_button);
    define("same-line", clapi_sameline);
    define("separator", clapi_separator);
    define("begin-group", clapi_begingroup);
    define("end-group", clapi_endgroup);
    define("push-item-width", clapi_pushitemwidth);
    define("pop-item-width", clapi_popitemwidth);
    define("collapsing-header", clapi_collapsingheader);
    define("spacing", clapi_spacing);
    define("tree-node", clapi_treenode);
    define("tree-pop", clapi_treepop);
    define("set-next-item-width", clapi_setnextitemwidth);
    define("push-style-var", clapi_pushstylevar);
    define("pop-style-var", clapi_popstylevar);
    define("columns", clapi_columns);
    define("push-id", clapi_pushid);
    define("pop-id", clapi_popid);
    define("align-text-to-frame-padding", clapi_aligntexttoframepadding);
    define("next-column", clapi_nextcolumn);
    define("set-next-window-size", clapi_setnextwindowsize);
    define("begin-child", clapi_beginchild);
    define("end-child", clapi_endchild);
    define("selectable", clapi_selectable);
    define("push-style-color", clapi_pushstylecolor);
    define("pop-style-color", clapi_popstylecolor);
    define("show-demo-window", clapi_showdemowindow);
    define("checkbox", clapi_checkbox);
    define("radio-button", clapi_radiobutton);
    define("item-hovered-p", clapi_isitemhovered);
    define("set-tooltip", clapi_settooltip);
    define("begin-tooltip", clapi_begintooltip);
    define("end-tooltip", clapi_endtooltip);
    define("get-clipboard-text", clapi_getclipboardtext);
    define("set-clipboard-text", clapi_setclipboardtext);
    define("begin-tab-bar", clapi_begintabbar);
    define("end-tab-bar", clapi_endtabbar);
    define("begin-tab-item", clapi_begintabitem);
    define("end-tab-item", clapi_endtabitem);
    define("set-tab-item-closed", clapi_settabitemclosed);
    define("set-item-default-focus", clapi_setitemdefaultfocus);
    define("set-keyboard-focus-here", clapi_setkeyboardfocushere);
    define("style-colors", clapi_stylecolors);
    define("begin-combo", clapi_begincombo);
    define("end-combo", clapi_endcombo);
    define("begin-listbox", clapi_beginlistbox);
    define("end-listbox", clapi_endlistbox);
    define("begin-main-menu-bar", clapi_beginmainmenubar);
    define("end-main-menu-bar", clapi_endmainmenubar);
    define("begin-menu-bar", clapi_beginmenubar);
    define("end-menu-bar", clapi_endmenubar);
    define("begin-menu", clapi_beginmenu);
    define("end-menu", clapi_endmenu);
    define("menu-item", clapi_menuitem);
    define("get-scroll-x", clapi_getscrollx);
    define("get-scroll-y", clapi_getscrollx);
    define("get-scroll-max-x", clapi_getscrollmaxx);
    define("get-scroll-max-y", clapi_getscrollmaxy);
    define("set-scroll-x", clapi_setscrollx);
    define("set-scroll-y", clapi_setscrolly);
    define("set-scroll-here-y", clapi_setscrollherey);
    define("set-scroll-from-pos-y", clapi_setscrollfromposy);
    define("slider-float", clapi_sliderfloat);
    define("slider-angle", clapi_sliderangle);
    define("slider-int", clapi_sliderint);
    define("vslider-float", clapi_vsliderfloat);
    define("vslider-int", clapi_vsliderint);
    define("window-appearing-p", clapi_iswindowappearing);
    define("window-collapsed-p", clapi_iswindowcollapsed);
    define("window-focused-p", clapi_iswindowfocused);
    define("window-hovered-p", clapi_iswindowhovered);
    define("get-window-pos", clapi_getwindowpos);
    define("get-window-size", clapi_getwindowsize);
    define("set-next-window-pos", clapi_setnextwindowpos);
    define("set-next-window-content-size", clapi_setnextwindowcontentsize);
    define("set-next-window-collapsed", clapi_setnextwindowcollapsed);
    define("set-next-window-focus", clapi_setnextwindowfocus);
    define("set-next-window-bg-alpha", clapi_setnextwindowbgalpha);
    define("set-window-pos", clapi_setwindowpos);
    define("set-window-size", clapi_setwindowsize);
    define("set-window-collapsed", clapi_setwindowcollapsed);
    define("set-window-focus", clapi_setwindowfocus);
    define("set-window-font-scale", clapi_setwindowfontscale);
    define("calc-item-width", clapi_calcitemwidth);
    define("push-text-wrap-pos", clapi_pushtextwrappos);
    define("pop-text-wrap-pos", clapi_poptextwrappos);
    define("push-allow-keyboard-focus", clapi_pushallowkeyboardfocus);
    define("pop-allow-keyboard-focus", clapi_popallowkeyboardfocus);
    define("push-button-repeat", clapi_pushbuttonrepeat);
    define("pop-button-repeat", clapi_popbuttonrepeat);
    define("new-line", clapi_newline);
    define("dummy", clapi_dummy);
    define("indent", clapi_indent);
    define("unindent", clapi_unindent);
    define("get-cursor-pos", clapi_getcursorpos);
    define("set-cursor-pos", clapi_setcursorpos);
    define("get-cursor-start-pos", clapi_getcursorstartpos);
    define("get-cursor-screen-pos", clapi_getcursorscreenpos);
    define("set-cursor-screen-pos", clapi_setcursorscreenpos);
    define("get-text-line-height", clapi_gettextlineheight);
    define("get-text-line-height-with-spacing", clapi_gettextlineheightwithspacing);
    define("get-frame-height", clapi_getframeheight);
    define("get-frame-height-with-spacing", clapi_getframeheightwithspacing);
    define("small-button", clapi_smallbutton);
    define("invisible-button", clapi_invisiblebutton);
    define("arrow-button", clapi_arrowbutton);
    define("bullet", clapi_bullet);
    define("get-content-region-max", clapi_getcontentregionmax);
    define("get-content-region-avail", clapi_getcontentregionavail);
    define("get-window-content-region-min", clapi_getwindowcontentregionmin);
    define("get-window-content-region-max", clapi_getwindowcontentregionmax);
    define("progress-bar", clapi_progressbar);
    define("item-active-p", clapi_isitemactive);
    define("item-focused-p", clapi_isitemfocused);
    define("item-clicked-p", clapi_isitemclicked);
    define("item-visible-p", clapi_isitemvisible);
    define("item-edited-p", clapi_isitemedited);
    define("item-activated-p", clapi_isitemactivated);
    define("item-deactivated-p", clapi_isitemdeactivated);
    define("item-deactivated-after-edit-p", clapi_isitemdeactivatedafteredit);
    define("any-item-hovered-p", clapi_isanyitemhovered);
    define("any-item-active-p", clapi_isanyitemactive);
    define("any-item-focused-p", clapi_isanyitemfocused);
    define("drag-float", clapi_dragfloat);
    define("input-float", clapi_inputfloat);
    define("drag-int", clapi_dragint);
    define("input-int", clapi_inputint);
}
