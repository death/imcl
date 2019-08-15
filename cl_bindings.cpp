#include "imgui.h"
#include <ecl/ecl.h>

// Support

static const char* asText(cl_object obj)
{
	return ecl_base_string_pointer_safe(si_coerce_to_base_string(cl_princ_to_string(obj)));
}

static int asInt(cl_object obj)
{
	return ecl_to_int32_t(ecl_truncate1(obj));
}

static float asFloat(cl_object obj)
{
	return ecl_to_float(obj);
}

static bool asBool(cl_object obj)
{
	return !Null(obj);
}

static cl_object asObject(cl_object obj)
{
	return obj;
}

#define POPARG(fn, def) (nargs > 0 ? nargs--, fn(ecl_va_arg(ap)) : (def))

#define RETFLOAT(x) result = ecl_make_single_float((x))

#define RETINT(x) result = ecl_make_int32_t((x))

#define RETBOOL(x) result = (x) ? ECL_T : ECL_NIL

#define APIFUNC(name)						\
	static cl_object clapi_ ## name(cl_narg nargs, ...)	\
	{							\
	ecl_va_list ap;						\
	cl_object result = ECL_NIL;				\
	ecl_va_start(ap, nargs, nargs, 0);

#define APIFUNC_END 				\
	ecl_va_end(ap);				\
	ecl_return1(ecl_process_env(), result); \
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

// The actual bindings

APIFUNC(begin)
{
    const char *label = POPARG(asText, "Window");
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
    const char *label = POPARG(asText, "Text");
    ImGui::Text(label);
}
APIFUNC_END

APIFUNC(button)
{
    const char *label = POPARG(asText, "Button");
    cl_object size = POPARG(asObject, ECL_NIL);

    ImVec2 sz(0, 0);
    ecl_imvec2_p(size, sz);

    bool ret = ImGui::Button(label, sz);
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
    int width = POPARG(asInt, -1);
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
    const char *label = POPARG(asText, "CollapsingHeader");
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
    const char *label = POPARG(asText, "BulletText");
    ImGui::BulletText(label);
}
APIFUNC_END

APIFUNC(treenode)
{
    const char *label = POPARG(asText, "TreeNode");
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
    int width = POPARG(asInt, -1);
    ImGui::SetNextItemWidth(width);
}
APIFUNC_END

APIFUNC(dragfloat)
{
    const char *name = POPARG(asText, "float");
    float f = POPARG(asFloat, 0.0F);
    ImGui::DragFloat(name, &f);
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
    cl_object whichvar = POPARG(asObject, ECL_NIL);
    if (ecl_keywordp(whichvar)) {
        struct cl_stylevar_entry *list = stylevarlist();
        int i;
        for (i = 0; i < ImGuiStyleVar_COUNT; i++) {
            if (ecl_equal(list[i].symbol, whichvar)) {
                break;
            }
        }
        if (i < ImGuiStyleVar_COUNT) {
            cl_object lispval = POPARG(asObject, ECL_NIL);
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
    int count = POPARG(asInt, 1);
    ImGui::PopStyleVar(count);
}
APIFUNC_END

APIFUNC(columns)
{
    // count int
    // id string
    // border bool
}
APIFUNC_END

APIFUNC(pushid)
{
    // id (string or int)
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
    // label string
    // v *float
    // step float
    // step-fast float
    // format string
    // flags ImGuiInputTextFlags
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
}
