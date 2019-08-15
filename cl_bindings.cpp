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
    if (cl_consp(size) != ECL_NIL) {
        cl_object car = cl_car(size);
        if (ecl_realp(car)) {
            cl_object cdr = cl_cdr(size);
            if (cl_consp(cdr) != ECL_NIL) {
                cl_object cadr = cl_car(cdr);
                if (ecl_realp(cadr)) {
                    cl_object cddr = cl_cdr(cdr);
                    if (cl_null(cddr) != ECL_NIL) {
                        sz.x = ecl_to_float(car);
                        sz.y = ecl_to_float(cadr);
                    }
                }
            }
        }
    }

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
}
