#include "imgui.h"
#include "imgui_impl_opengl3.h"
#include <ecl/ecl.h>
#include <string>

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

#define RETDOUBLE(x) result = ecl_make_double_float((x))

#define RETINT(x) result = ecl_make_int32_t((x))

#define RETUINT(x) result = ecl_make_uint32_t((x))

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

// Our gui package

cl_object imcl_package()
{
    static cl_object package = ECL_NIL;
    if (package == ECL_NIL) {
        package = ecl_make_package(ecl_cstring_to_base_string_or_nil("IMCL"),
                                   cl_list(1, ecl_cstring_to_base_string_or_nil("UI")),
                                   ecl_list1(cl_core.lisp_package),
                                   ECL_NIL);
    }
    return package;
}

static cl_object intern(const char *name)
{
    cl_object package = imcl_package();
    return _ecl_intern(name, package);
}

cl_object imcl_intern_and_export(const char *name)
{
    cl_object package = imcl_package();
    cl_object symbol = _ecl_intern(name, package);
    cl_export2(symbol, package);
    return symbol;
}

static cl_object define(const char *name, cl_objectfn fn)
{
    cl_object symbol = imcl_intern_and_export(name);
    // New ECL 20.4.24 (yay!) had a breaking change adding a parameter
    // for specifying the number of fixed arguments.  The commit
    // message for that change says that it's due to ARM64 calling
    // conventions.  Rather than go through each function and count
    // the number of required arguments, I decided to pass zero (too
    // bad for iOS).
    ecl_def_c_function_va(symbol, fn, 0);
    return symbol;
}

// Translating keywords to enum values and vice versa

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

cl_object enum_value_keyword(int value,
                             struct keyword_enum_descriptor *descriptors,
                             int size)
{
    for (int i = 0; i < size; i++) {
        if (descriptors[i].value == value) {
            return ecl_make_keyword(descriptors[i].keyword_name);
        }
    }
    FEerror("value not in enum descriptors list", 0);
    return ECL_NIL;
}

int keyword_flags_value(cl_object flags, struct keyword_enum_descriptor *descriptors, int size)
{
    if (ecl_keywordp(flags)) {
        return keyword_enum_value(flags, descriptors, size);
    } else if (CONSP(flags)) {
        int value = 0;
        while (CONSP(flags)) {
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
    if (!CONSP(object)) {
        return false;
    }
    cl_object car = cl_car(object);
    if (!ecl_realp(car)) {
        return false;
    }
    cl_object cdr = cl_cdr(object);
    if (!CONSP(object)) {
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
    if (!CONSP(object)) {
        return false;
    }
    cl_object car = cl_car(object);
    if (!ecl_realp(car)) {
        return false;
    }
    cl_object cdr = cl_cdr(object);
    if (!CONSP(cdr)) {
        return false;
    }
    cl_object cadr = cl_car(cdr);
    if (!ecl_realp(cadr)) {
        return false;
    }
    cl_object cddr = cl_cdr(cdr);
    if (!CONSP(cddr)) {
        return false;
    }
    cl_object caddr = cl_car(cddr);
    if (!ecl_realp(caddr)) {
        return false;
    }
    cl_object cdddr = cl_cdr(cddr);
    if (!CONSP(cdddr)) {
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

struct keyword_enum_descriptor imguicoloreditflags[] = {
    {"NONE", ImGuiColorEditFlags_None},
    {"NO-ALPHA", ImGuiColorEditFlags_NoAlpha},
    {"NO-PICKER", ImGuiColorEditFlags_NoPicker},
    {"NO-OPTIONS", ImGuiColorEditFlags_NoOptions},
    {"NO-SMALL-PREVIEW", ImGuiColorEditFlags_NoSmallPreview},
    {"NO-INPUTS", ImGuiColorEditFlags_NoInputs},
    {"NO-TOOLTIP", ImGuiColorEditFlags_NoTooltip},
    {"NO-LABEL", ImGuiColorEditFlags_NoLabel},
    {"NO-SIDE-PREVIEW", ImGuiColorEditFlags_NoSidePreview},
    {"NO-DRAG-DROP", ImGuiColorEditFlags_NoDragDrop},
    {"ALPHA-BAR", ImGuiColorEditFlags_AlphaBar},
    {"ALPHA-PREVIEW", ImGuiColorEditFlags_AlphaPreview},
    {"ALPHA-PREVIEW-HALF", ImGuiColorEditFlags_AlphaPreviewHalf},
    {"HDR", ImGuiColorEditFlags_HDR},
    {"DISPLAY-RGB", ImGuiColorEditFlags_DisplayRGB},
    {"DISPLAY-HSV", ImGuiColorEditFlags_DisplayHSV},
    {"DISPLAY-HEX", ImGuiColorEditFlags_DisplayHex},
    {"UINT8", ImGuiColorEditFlags_Uint8},
    {"FLOAT", ImGuiColorEditFlags_Float},
    {"PICKER-HUE-BAR", ImGuiColorEditFlags_PickerHueBar},
    {"PICKER-HUE-WHEEL", ImGuiColorEditFlags_PickerHueWheel},
    {"INPUT-RGB", ImGuiColorEditFlags_InputRGB},
    {"INPUT-HSV", ImGuiColorEditFlags_InputHSV},
    {"OPTIONS-DEFAULT", ImGuiColorEditFlags__OptionsDefault},
};

ImGuiColorEditFlags as_imguicoloreditflags(cl_object obj)
{
    ImGuiColorEditFlags flags = ImGuiColorEditFlags_None;
    if (obj != ECL_NIL) {
        flags = keyword_flags_value(obj, imguicoloreditflags, LENGTHOF(imguicoloreditflags));
    }
    return flags;
}

struct keyword_enum_descriptor imguitreenodeflags[] = {
    {"NONE", ImGuiTreeNodeFlags_None},
    {"SELECTED", ImGuiTreeNodeFlags_Selected},
    {"FRAMED", ImGuiTreeNodeFlags_Framed},
    {"ALLOW-ITEM-OVERLAP", ImGuiTreeNodeFlags_AllowItemOverlap},
    {"NO-TREE-PUSH-ON-OPEN", ImGuiTreeNodeFlags_NoTreePushOnOpen},
    {"NO-AUTO-OPEN-ON-LOG", ImGuiTreeNodeFlags_NoAutoOpenOnLog},
    {"DEFAULT-OPEN", ImGuiTreeNodeFlags_DefaultOpen},
    {"OPEN-ON-DOUBLE-CLICK", ImGuiTreeNodeFlags_OpenOnDoubleClick},
    {"OPEN-ON-ARROW", ImGuiTreeNodeFlags_OpenOnArrow},
    {"LEAF", ImGuiTreeNodeFlags_Leaf},
    {"BULLET", ImGuiTreeNodeFlags_Bullet},
    {"FRAME-PADDING", ImGuiTreeNodeFlags_FramePadding},
    {"NAV-LEFT-JUMPS-BACK-HERE", ImGuiTreeNodeFlags_NavLeftJumpsBackHere},
    {"COLLAPSING-HEADER", ImGuiTreeNodeFlags_CollapsingHeader},
};

ImGuiTreeNodeFlags as_imguitreenodeflags(cl_object obj)
{
    ImGuiTreeNodeFlags flags = ImGuiTreeNodeFlags_None;
    if (obj != ECL_NIL) {
        flags = keyword_flags_value(obj, imguitreenodeflags, LENGTHOF(imguitreenodeflags));
    }
    return flags;
}

struct keyword_enum_descriptor imguitabbarflags[] = {
    {"NONE", ImGuiTabBarFlags_None},
    {"REORDERABLE", ImGuiTabBarFlags_Reorderable},
    {"AUTO-SELECT-NEW-TABS", ImGuiTabBarFlags_AutoSelectNewTabs},
    {"TAB-LIST-POPUP-BUTTON", ImGuiTabBarFlags_TabListPopupButton},
    {"NO-CLOSE-WITH-MIDDLE-MOUSE-BUTTON", ImGuiTabBarFlags_NoCloseWithMiddleMouseButton},
    {"NO-TAB-LIST-SCROLLING-BUTTONS", ImGuiTabBarFlags_NoTabListScrollingButtons},
    {"NO-TOOLTIP", ImGuiTabBarFlags_NoTooltip},
    {"FITTING-POLICY-RESIZEDOWN", ImGuiTabBarFlags_FittingPolicyResizeDown},
    {"FITTING-POLICY-SCROLL", ImGuiTabBarFlags_FittingPolicyScroll},
    {"FITTING-POLICY-DEFAULT", ImGuiTabBarFlags_FittingPolicyDefault_},
};

ImGuiTabBarFlags as_imguitabbarflags(cl_object obj)
{
    ImGuiTabBarFlags flags = ImGuiTabBarFlags_None;
    if (obj != ECL_NIL) {
        flags = keyword_flags_value(obj, imguitabbarflags, LENGTHOF(imguitabbarflags));
    }
    return flags;
}

struct keyword_enum_descriptor imguitabitemflags[] = {
    {"NONE", ImGuiTabItemFlags_None},
    {"UNSAVED-DOCUMENT", ImGuiTabItemFlags_UnsavedDocument},
    {"SET-SELECTED", ImGuiTabItemFlags_SetSelected},
    {"NO-CLOSE-WITH-MIDDLE-MOUSE-BUTTON", ImGuiTabItemFlags_NoCloseWithMiddleMouseButton},
    {"NO-PUSH-ID", ImGuiTabItemFlags_NoPushId},
};

ImGuiTabBarFlags as_imguitabitemflags(cl_object obj)
{
    ImGuiTabItemFlags flags = ImGuiTabItemFlags_None;
    if (obj != ECL_NIL) {
        flags = keyword_flags_value(obj, imguitabitemflags, LENGTHOF(imguitabitemflags));
    }
    return flags;
}

struct keyword_enum_descriptor imguikey[] = {
    {"TAB", ImGuiKey_Tab},
    {"LEFT-ARROW", ImGuiKey_LeftArrow},
    {"RIGHT-ARROW", ImGuiKey_RightArrow},
    {"UP-ARROW", ImGuiKey_UpArrow},
    {"DOWN-ARROW", ImGuiKey_DownArrow},
    {"PAGE-UP", ImGuiKey_PageUp},
    {"PAGE-DOWN", ImGuiKey_PageDown},
    {"HOME", ImGuiKey_Home},
    {"END", ImGuiKey_End},
    {"INSERT", ImGuiKey_Insert},
    {"DELETE", ImGuiKey_Delete},
    {"BACKSPACE", ImGuiKey_Backspace},
    {"SPACE", ImGuiKey_Space},
    {"ENTER", ImGuiKey_Enter},
    {"ESCAPE", ImGuiKey_Escape},
    {"A", ImGuiKey_A},
    {"C", ImGuiKey_C},
    {"V", ImGuiKey_V},
    {"X", ImGuiKey_X},
    {"Y", ImGuiKey_Y},
    {"Z", ImGuiKey_Z},
};

ImGuiKey as_imguikey(cl_object obj)
{
    ImGuiKey key = ImGuiKey_Enter;
    if (obj != ECL_NIL) {
        key = keyword_enum_value(obj, imguikey, LENGTHOF(imguikey));
    }
    return key;
}

struct keyword_enum_descriptor imguimousecursor[] = {
    {"NONE", ImGuiMouseCursor_None},
    {"ARROW", ImGuiMouseCursor_Arrow},
    {"TEXT-INPUT", ImGuiMouseCursor_TextInput},
    {"RESIZE-ALL", ImGuiMouseCursor_ResizeAll},
    {"RESIZE-NS", ImGuiMouseCursor_ResizeNS},
    {"RESIZE-EW", ImGuiMouseCursor_ResizeEW},
    {"RESIZE-NESW", ImGuiMouseCursor_ResizeNESW},
    {"RESIZE-NWSE", ImGuiMouseCursor_ResizeNWSE},
    {"HAND", ImGuiMouseCursor_Hand},
};

ImGuiMouseCursor as_imguimousecursor(cl_object obj)
{
    ImGuiMouseCursor cursor = ImGuiMouseCursor_Arrow;
    if (obj != ECL_NIL) {
        cursor = keyword_enum_value(obj, imguimousecursor, LENGTHOF(imguimousecursor));
    }
    return cursor;
}

struct keyword_enum_descriptor imguicomboflags[] = {
    {"NONE", ImGuiComboFlags_None},
    {"POPUP-ALIGN-LEFT", ImGuiComboFlags_PopupAlignLeft},
    {"HEIGHT-SMALL", ImGuiComboFlags_HeightSmall},
    {"HEIGHT-REGULAR", ImGuiComboFlags_HeightRegular},
    {"HEIGHT-LARGE", ImGuiComboFlags_HeightLarge},
    {"HEIGHT-LARGEST", ImGuiComboFlags_HeightLargest},
    {"NO-ARROW-BUTTON", ImGuiComboFlags_NoArrowButton},
    {"NO-PREVIEW", ImGuiComboFlags_NoPreview},
};

ImGuiTabBarFlags as_imguicomboflags(cl_object obj)
{
    ImGuiComboFlags flags = ImGuiComboFlags_None;
    if (obj != ECL_NIL) {
        flags = keyword_flags_value(obj, imguicomboflags, LENGTHOF(imguicomboflags));
    }
    return flags;
}

struct keyword_enum_descriptor imguidragdropflags[] = {
    {"NONE", ImGuiDragDropFlags_None},
    {"SOURCE-NO-PREVIEW-TOOLTIP", ImGuiDragDropFlags_SourceNoPreviewTooltip},
    {"SOURCE-NO-DISABLE-HOVER", ImGuiDragDropFlags_SourceNoDisableHover},
    {"SOURCE-NO-HOLD-TO-OPEN-OTHERS", ImGuiDragDropFlags_SourceNoHoldToOpenOthers},
    {"SOURCE-ALLOW-NULL-ID", ImGuiDragDropFlags_SourceAllowNullID},
    {"SOURCE-EXTERN", ImGuiDragDropFlags_SourceExtern},
    {"SOURCE-AUTO-EXPIRE-PAYLOAD", ImGuiDragDropFlags_SourceAutoExpirePayload},
    {"ACCEPT-BEFORE-DELIVERY", ImGuiDragDropFlags_AcceptBeforeDelivery},
    {"ACCEPT-NO-DRAW-DEFAULT-RECT", ImGuiDragDropFlags_AcceptNoDrawDefaultRect},
    {"ACCEPT-NO-PREVIEW-TOOLTIP", ImGuiDragDropFlags_AcceptNoPreviewTooltip},
    {"ACCEPT-PEEK-ONLY", ImGuiDragDropFlags_AcceptPeekOnly},
};

ImGuiDragDropFlags as_imguidragdropflags(cl_object obj)
{
    ImGuiDragDropFlags flags = ImGuiDragDropFlags_None;
    if (obj != ECL_NIL) {
        flags = keyword_flags_value(obj, imguidragdropflags, LENGTHOF(imguidragdropflags));
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
    ImGuiTreeNodeFlags flags = POPARG(as_imguitreenodeflags, ImGuiTreeNodeFlags_None);
    bool ret = ImGui::CollapsingHeader(label, flags);
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
    ImGuiTreeNodeFlags flags = POPARG(as_imguitreenodeflags, ImGuiTreeNodeFlags_None);
    bool ret = ImGui::TreeNodeEx(label, flags);
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
    ImGuiTabBarFlags flags = POPARG(as_imguitabbarflags, ImGuiTabBarFlags_None);
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
    ImGuiTabItemFlags flags = POPARG(as_imguitabitemflags, ImGuiTabItemFlags_None);
    // TODO: p_open arg
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
    ImGuiComboFlags flags = POPARG(as_imguicomboflags, ImGuiComboFlags_None);
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
    while (CONSP(sub) && n < 4) {
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
    if (CONSP(val)) {
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
    while (CONSP(sub) && n < 4) {
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
    if (CONSP(val)) {
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
    if (CONSP(val)) {
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
    while (CONSP(sub) && n < 4) {
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
    while (CONSP(sub) && n < 4) {
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
    while (CONSP(sub) && n < 4) {
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
    while (CONSP(sub) && n < 4) {
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

APIFUNC(getcolumnindex)
{
    int ret = ImGui::GetColumnIndex();
    RETINT(ret);
}
APIFUNC_END

APIFUNC(getcolumnwidth)
{
    int index = POPARG(as_int, -1);
    float ret = ImGui::GetColumnWidth(index);
    RETFLOAT(ret);
}
APIFUNC_END

APIFUNC(setcolumnwidth)
{
    int index = POPARG(as_int, -1);
    float width = POPARG(as_float, 1.0F);
    ImGui::SetColumnWidth(index, width);
}
APIFUNC_END

APIFUNC(getcolumnoffset)
{
    int index = POPARG(as_int, -1);
    float ret = ImGui::GetColumnOffset(index);
    RETFLOAT(ret);
}
APIFUNC_END

APIFUNC(setcolumnoffset)
{
    int index = POPARG(as_int, -1);
    float offset = POPARG(as_float, 0.0F);
    ImGui::SetColumnOffset(index, offset);
}
APIFUNC_END

APIFUNC(getcolumnscount)
{
    int ret = ImGui::GetColumnsCount();
    RETINT(ret);
}
APIFUNC_END

APIFUNC(showaboutwindow)
{
    bool open = true;
    ImGui::ShowAboutWindow(&open);
    RETBOOL(open);
}
APIFUNC_END

APIFUNC(showmetricswindow)
{
    bool open = true;
    ImGui::ShowMetricsWindow(&open);
    RETBOOL(open);
}
APIFUNC_END

APIFUNC(showstyleeditor)
{
    // TODO: take style argument
    ImGuiStyle *style = NULL;
    ImGui::ShowStyleEditor(style);
}
APIFUNC_END

APIFUNC(showstyleselector)
{
    const char *label = POPARG(as_text, "label");
    bool ret = ImGui::ShowStyleSelector(label);
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(showfontselector)
{
    const char *label = POPARG(as_text, "label");
    ImGui::ShowFontSelector(label);
}
APIFUNC_END

APIFUNC(showuserguide)
{
    ImGui::ShowUserGuide();
}
APIFUNC_END

APIFUNC(getversion)
{
    const char *version = ImGui::GetVersion();
    RETSTRING(version);
}
APIFUNC_END

APIFUNC2(getitemrectmin)
{
    ImVec2 ret = ImGui::GetItemRectMin();
    RETIMVEC2(ret);
}
APIFUNC2_END

APIFUNC2(getitemrectmax)
{
    ImVec2 ret = ImGui::GetItemRectMax();
    RETIMVEC2(ret);
}
APIFUNC2_END

APIFUNC2(getitemrectsize)
{
    ImVec2 ret = ImGui::GetItemRectSize();
    RETIMVEC2(ret);
}
APIFUNC2_END

APIFUNC(setitemallowoverlap)
{
    ImGui::SetItemAllowOverlap();
}
APIFUNC_END

APIFUNC(coloredit)
{
    const char *label = POPARG(as_text, "label");
    cl_object val = POPARG(as_object, ECL_NIL);
    ImGuiColorEditFlags flags = POPARG(as_imguicoloreditflags, ImGuiColorEditFlags_None);
    bool ret = false;
    float v[4];
    int n = 0;
    cl_object sub = val;
    while (CONSP(sub) && n < 4) {
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
    case 3:
        ret = ImGui::ColorEdit3(label, v, flags);
        break;
    case 4:
        ret = ImGui::ColorEdit4(label, v, flags);
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

APIFUNC(colorpicker)
{
    const char *label = POPARG(as_text, "label");
    cl_object val = POPARG(as_object, ECL_NIL);
    ImGuiColorEditFlags flags = POPARG(as_imguicoloreditflags, ImGuiColorEditFlags_None);
    bool ret = false;
    float v[4];
    int n = 0;
    cl_object sub = val;
    while (CONSP(sub) && n < 4) {
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
    case 3:
        ret = ImGui::ColorPicker3(label, v, flags);
        break;
    case 4:
        // TODO: ref_col
        ret = ImGui::ColorPicker4(label, v, flags, NULL);
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

APIFUNC(colorbutton)
{
    const char *desc_id = POPARG(as_text, "desc");
    ImVec4 col = POPARG(as_imvec4, ImVec4(0, 0, 0, 0));
    ImGuiColorEditFlags flags = POPARG(as_imguicoloreditflags, ImGuiColorEditFlags_None);
    ImVec2 size = POPARG(as_imvec2, ImVec2(0, 0));
    bool ret = ImGui::ColorButton(desc_id, col, flags, size);
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(setcoloreditoptions)
{
    ImGuiColorEditFlags flags = POPARG(as_imguicoloreditflags, ImGuiColorEditFlags__OptionsDefault);
    ImGui::SetColorEditOptions(flags);
}
APIFUNC_END

APIFUNC(isrectvisible)
{
    bool ret = false;
    if (nargs == 1) {
        ImVec2 size = POPARG(as_imvec2, ImVec2(0, 0));
        ret = ImGui::IsRectVisible(size);
    } else if (nargs == 2) {
        ImVec2 rect_min = POPARG(as_imvec2, ImVec2(0, 0));
        ImVec2 rect_max = POPARG(as_imvec2, ImVec2(0, 0));
        ret = ImGui::IsRectVisible(rect_min, rect_max);
    }
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(gettime)
{
    double time = ImGui::GetTime();
    RETDOUBLE(time);
}
APIFUNC_END

APIFUNC(getframecount)
{
    int ret = ImGui::GetFrameCount();
    RETINT(ret);
}
APIFUNC_END

APIFUNC2(calctextsize)
{
    const char *text = POPARG(as_text, "");
    bool just_label_part = POPARG(as_bool, false);
    float wrap_width = POPARG(as_float, -1.0F);
    ImVec2 ret = ImGui::CalcTextSize(text, NULL, just_label_part, wrap_width);
    RETIMVEC2(ret);
}
APIFUNC2_END

APIFUNC2(calclistclipping)
{
    int nitems = POPARG(as_int, INT_MAX);
    float height = POPARG(as_float, 1.0F);
    int display_start = 0;
    int display_end = 0;
    ImGui::CalcListClipping(nitems, height, &display_start, &display_end);
    result1 = ecl_make_int32_t(display_start);
    result2 = ecl_make_int32_t(display_end);
}
APIFUNC2_END

APIFUNC(treepush)
{
    const char *str_id = POPARG(as_text, "id");
    ImGui::TreePush(str_id);
}
APIFUNC_END

APIFUNC(treeadvancetolabelpos)
{
    ImGui::TreeAdvanceToLabelPos();
}
APIFUNC_END

APIFUNC(gettreenodetolabelspacing)
{
    float ret = ImGui::GetTreeNodeToLabelSpacing();
    RETFLOAT(ret);
}
APIFUNC_END

APIFUNC(setnexttreenodeopen)
{
    bool is_open = POPARG(as_bool, true);
    ImGuiCond cond = POPARG(as_imguicond, ImGuiCond_Always);
    ImGui::SetNextTreeNodeOpen(is_open, cond);
}
APIFUNC_END

APIFUNC(openpopup)
{
    const char *str_id = POPARG(as_text, "id");
    ImGui::OpenPopup(str_id);
}
APIFUNC_END

APIFUNC(beginpopup)
{
    const char *str_id = POPARG(as_text, "id");
    ImGuiWindowFlags flags = POPARG(as_imguiwindowflags, ImGuiWindowFlags_None);
    bool ret = ImGui::BeginPopup(str_id, flags);
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(beginpopupcontextitem)
{
    const char *str_id = POPARG(as_text, NULL);
    int mouse_button = POPARG(as_int, 1);
    bool ret = ImGui::BeginPopupContextItem(str_id, mouse_button);
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(beginpopupcontextwindow)
{
    const char *str_id = POPARG(as_text, NULL);
    int mouse_button = POPARG(as_int, 1);
    bool also_over_items = POPARG(as_bool, true);
    bool ret = ImGui::BeginPopupContextWindow(str_id, mouse_button, also_over_items);
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(beginpopupcontextvoid)
{
    const char *str_id = POPARG(as_text, NULL);
    int mouse_button = POPARG(as_int, 1);
    bool ret = ImGui::BeginPopupContextVoid(str_id, mouse_button);
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(beginpopupmodal)
{
    const char *name = POPARG(as_text, "name");
    ImGuiWindowFlags flags = POPARG(as_imguiwindowflags, ImGuiWindowFlags_None);
    // TODO: figure out p_open
    bool ret = ImGui::BeginPopupModal(name, NULL, flags);
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(endpopup)
{
    ImGui::EndPopup();
}
APIFUNC_END

APIFUNC(openpopuponitemclick)
{
    const char *str_id = POPARG(as_text, NULL);
    int mouse_button = POPARG(as_int, 1);
    bool ret = ImGui::OpenPopupOnItemClick(str_id, mouse_button);
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(ispopupopen)
{
    const char *str_id = POPARG(as_text, "id");
    bool ret = ImGui::IsPopupOpen(str_id);
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(closecurrentpopup)
{
    ImGui::CloseCurrentPopup();
}
APIFUNC_END

APIFUNC(pushcliprect)
{
    ImVec2 rect_min = POPARG(as_imvec2, ImVec2(0, 0));
    ImVec2 rect_max = POPARG(as_imvec2, ImVec2(0, 0));
    bool intersect = POPARG(as_bool, true);
    ImGui::PushClipRect(rect_min, rect_max, intersect);
}
APIFUNC_END

APIFUNC(popcliprect)
{
    ImGui::PopClipRect();
}
APIFUNC_END

APIFUNC(getid)
{
    const char *str_id = POPARG(as_text, "id");
    ImGuiID id = ImGui::GetID(str_id);
    RETUINT(id);
}
APIFUNC_END

APIFUNC(beginchildframe)
{
    ImGuiID id = POPARG(as_uint, 0);
    ImVec2 size = POPARG(as_imvec2, ImVec2(0, 0));
    ImGuiWindowFlags flags = POPARG(as_imguiwindowflags, ImGuiWindowFlags_None);
    bool ret = ImGui::BeginChildFrame(id, size, flags);
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(endchildframe)
{
    ImGui::EndChildFrame();
}
APIFUNC_END

APIFUNC(rgbtohsv)
{
    cl_object rgb = POPARG(as_object, ECL_NIL);
    cl_object ret = ECL_NIL;
    if (rgb != ECL_NIL) {
        float r = as_float(cl_first(rgb));
        float g = as_float(cl_second(rgb));
        float b = as_float(cl_third(rgb));
        float h = 0.0F;
        float s = 0.0F;
        float v = 0.0F;
        ImGui::ColorConvertRGBtoHSV(r, g, b, h, s, v);
        ret = cl_list(3,
                      ecl_make_single_float(h),
                      ecl_make_single_float(s),
                      ecl_make_single_float(v));
    }
    result = ret;
}
APIFUNC_END

APIFUNC(hsvtorgb)
{
    cl_object hsv = POPARG(as_object, ECL_NIL);
    cl_object ret = ECL_NIL;
    if (hsv != ECL_NIL) {
        float h = as_float(cl_first(hsv));
        float s = as_float(cl_second(hsv));
        float v = as_float(cl_third(hsv));
        float r = 0.0F;
        float g = 0.0F;
        float b = 0.0F;
        ImGui::ColorConvertHSVtoRGB(h, s, v, r, g, b);
        ret = cl_list(3,
                      ecl_make_single_float(r),
                      ecl_make_single_float(g),
                      ecl_make_single_float(b));
    }
    result = ret;
}
APIFUNC_END

APIFUNC(getkeyindex)
{
    ImGuiKey key = POPARG(as_imguikey, ImGuiKey_Enter);
    int ret = ImGui::GetKeyIndex(key);
    RETINT(ret);
}
APIFUNC_END

APIFUNC(iskeydown)
{
    int index = POPARG(as_int, 0);
    bool ret = ImGui::IsKeyDown(index);
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(iskeypressed)
{
    int index = POPARG(as_int, 0);
    bool repeat = POPARG(as_bool, true);
    bool ret = ImGui::IsKeyPressed(index, repeat);
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(iskeyreleased)
{
    int index = POPARG(as_int, 0);
    bool ret = ImGui::IsKeyPressed(index);
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(getkeypressedamount)
{
    int index = POPARG(as_int, 0);
    float delay = POPARG(as_float, 1.0F);
    float rate = POPARG(as_float, 0.1F);
    int ret = ImGui::GetKeyPressedAmount(index, delay, rate);
    RETINT(ret);
}
APIFUNC_END

APIFUNC(ismousedown)
{
    int button = POPARG(as_int, 0);
    bool ret = ImGui::IsMouseDown(button);
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(isanymousedown)
{
    bool ret = ImGui::IsAnyMouseDown();
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(ismouseclicked)
{
    int button = POPARG(as_int, 0);
    bool repeat = POPARG(as_bool, false);
    bool ret = ImGui::IsMouseClicked(button, repeat);
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(ismousedoubleclicked)
{
    int button = POPARG(as_int, 0);
    bool ret = ImGui::IsMouseDoubleClicked(button);
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(ismousereleased)
{
    int button = POPARG(as_int, 0);
    bool ret = ImGui::IsMouseReleased(button);
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(ismousedragging)
{
    int button = POPARG(as_int, 0);
    float lock_threshold = POPARG(as_float, -1.0F);
    bool ret = ImGui::IsMouseDragging(button, lock_threshold);
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(ismousehoveringrect)
{
    ImVec2 rect_min = POPARG(as_imvec2, ImVec2(0, 0));
    ImVec2 rect_max = POPARG(as_imvec2, ImVec2(0, 0));
    bool clip = POPARG(as_bool, true);
    bool ret = ImGui::IsMouseHoveringRect(rect_min, rect_max, clip);
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(ismouseposvalid)
{
    ImVec2 pos = POPARG(as_imvec2, ImVec2(-FLT_MAX, -FLT_MAX));
    bool ret = ImGui::IsMousePosValid(&pos);
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC2(getmousepos)
{
    ImVec2 pos = ImGui::GetMousePos();
    if (!ImGui::IsMousePosValid(&pos)) {
        // Nicer than -FLT_MAX.
        pos.x = pos.y = -1.0F;
    }
    RETIMVEC2(pos);
}
APIFUNC2_END

APIFUNC2(getmouseposonopeningcurrentpopup)
{
    ImVec2 pos = ImGui::GetMousePosOnOpeningCurrentPopup();
    RETIMVEC2(pos);
}
APIFUNC2_END

APIFUNC2(getmousedragdelta)
{
    int button = POPARG(as_int, 0);
    float lock_threshold = POPARG(as_float, -1.0F);
    ImVec2 delta = ImGui::GetMouseDragDelta(button, lock_threshold);
    RETIMVEC2(delta);
}
APIFUNC2_END

APIFUNC(resetmousedragdelta)
{
    int button = POPARG(as_int, 0);
    ImGui::ResetMouseDragDelta(button);
}
APIFUNC_END

APIFUNC(getmousecursor)
{
    ImGuiMouseCursor cursor = ImGui::GetMouseCursor();
    result = enum_value_keyword(cursor, imguimousecursor, LENGTHOF(imguimousecursor));
}
APIFUNC_END

APIFUNC(setmousecursor)
{
    ImGuiMouseCursor cursor = POPARG(as_imguimousecursor, ImGuiMouseCursor_Arrow);
    ImGui::SetMouseCursor(cursor);
}
APIFUNC_END

APIFUNC(capturekeyboardfromapp)
{
    bool capture = POPARG(as_bool, true);
    ImGui::CaptureKeyboardFromApp(capture);
}
APIFUNC_END

APIFUNC(capturemousefromapp)
{
    bool capture = POPARG(as_bool, true);
    ImGui::CaptureMouseFromApp(capture);
}
APIFUNC_END

struct InputTextCallback_UserData
{
    std::string s;
};

int InputTextCallback(ImGuiInputTextCallbackData *data)
{
    InputTextCallback_UserData *userdata = (InputTextCallback_UserData *)data->UserData;

    if (data->EventFlag == ImGuiInputTextFlags_CallbackResize) {
        userdata->s.resize(data->BufTextLen);
        data->Buf = (char *)userdata->s.c_str();
    }

    return 0;
}

APIFUNC(inputtext)
{
    bool ret = false;
    const char *label = POPARG(as_text, "label");
    cl_object stringbox = POPARG(as_object, ECL_NIL);
    if (CONSP(stringbox)) {
        cl_object s = cl_car(stringbox);
        if (cl_stringp(s) != ECL_NIL) {
            ImGuiInputTextFlags flags = POPARG(as_imguiinputtextflags, ImGuiInputTextFlags_None);
            flags |= ImGuiInputTextFlags_CallbackResize;
            InputTextCallback_UserData userdata;
            std::string cpp_s = as_text(s);
            userdata.s = cpp_s;
            ret = ImGui::InputText(label,
                                   (char *)userdata.s.c_str(),
                                   userdata.s.capacity() + 1,
                                   flags,
                                   InputTextCallback,
                                   &userdata);
            if (userdata.s != cpp_s) {
                s = ecl_make_simple_base_string(userdata.s.c_str(), -1);
                cl_rplaca(stringbox, s);
            }
        }
    }
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(inputtextmultiline)
{
    bool ret = false;
    const char *label = POPARG(as_text, "label");
    cl_object stringbox = POPARG(as_object, ECL_NIL);
    if (CONSP(stringbox)) {
        cl_object s = cl_car(stringbox);
        if (cl_stringp(s) != ECL_NIL) {
            ImVec2 size = POPARG(as_imvec2, ImVec2(0, 0));
            ImGuiInputTextFlags flags = POPARG(as_imguiinputtextflags, ImGuiInputTextFlags_None);
            flags |= ImGuiInputTextFlags_CallbackResize;
            InputTextCallback_UserData userdata;
            std::string cpp_s = as_text(s);
            userdata.s = cpp_s;
            ret = ImGui::InputTextMultiline(label,
                                            (char *)userdata.s.c_str(),
                                            userdata.s.capacity() + 1,
                                            size,
                                            flags,
                                            InputTextCallback,
                                            &userdata);
            if (userdata.s != cpp_s) {
                s = ecl_make_simple_base_string(userdata.s.c_str(), -1);
                cl_rplaca(stringbox, s);
            }
        }
    }
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(inputtextwithhint)
{
    bool ret = false;
    const char *label = POPARG(as_text, "label");
    const char *hint = POPARG(as_text, "hint");
    cl_object stringbox = POPARG(as_object, ECL_NIL);
    if (CONSP(stringbox)) {
        cl_object s = cl_car(stringbox);
        if (cl_stringp(s) != ECL_NIL) {
            ImGuiInputTextFlags flags = POPARG(as_imguiinputtextflags, ImGuiInputTextFlags_None);
            flags |= ImGuiInputTextFlags_CallbackResize;
            InputTextCallback_UserData userdata;
            std::string cpp_s = as_text(s);
            userdata.s = cpp_s;
            ret = ImGui::InputTextWithHint(label,
                                           hint,
                                           (char *)userdata.s.c_str(),
                                           userdata.s.capacity() + 1,
                                           flags,
                                           InputTextCallback,
                                           &userdata);
            if (userdata.s != cpp_s) {
                s = ecl_make_simple_base_string(userdata.s.c_str(), -1);
                cl_rplaca(stringbox, s);
            }
        }
    }
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(dragfloatrange)
{
    bool ret = false;
    const char *label = POPARG(as_text, "label");
    cl_object val = POPARG(as_object, ECL_NIL);
    if (CONSP(val) && ecl_length(val) == 2) {
        float v_speed = POPARG(as_float, 1.0F);
        float v_min = POPARG(as_float, 0.0F);
        float v_max = POPARG(as_float, 0.0F);
        const char *format = POPARG(as_text, "%.3f");
        const char *format_max = POPARG(as_text, NULL);
        float power = POPARG(as_float, 1.0F);
        float v[2];
        v[0] = as_float(cl_first(val));
        v[1] = as_float(cl_second(val));
        ret = ImGui::DragFloatRange2(label, &v[0], &v[1], v_speed, v_min, v_max, format, format_max, power);
        cl_rplaca(val, ecl_make_single_float(v[0]));
        cl_rplaca(cl_cdr(val), ecl_make_single_float(v[1]));
    }
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(dragintrange)
{
    bool ret = false;
    const char *label = POPARG(as_text, "label");
    cl_object val = POPARG(as_object, ECL_NIL);
    if (CONSP(val) && ecl_length(val) == 2) {
        float v_speed = POPARG(as_float, 1.0F);
        int v_min = POPARG(as_int, 0);
        int v_max = POPARG(as_int, 0);
        const char *format = POPARG(as_text, "%d");
        const char *format_max = POPARG(as_text, NULL);
        int v[2];
        v[0] = as_int(cl_first(val));
        v[1] = as_int(cl_second(val));
        ret = ImGui::DragIntRange2(label, &v[0], &v[1], v_speed, v_min, v_max, format, format_max);
        cl_rplaca(val, ecl_make_int32_t(v[0]));
        cl_rplaca(cl_cdr(val), ecl_make_int32_t(v[1]));
    }
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(plotlines)
{
    const char *label = POPARG(as_text, "label");
    cl_object val = POPARG(as_object, ECL_NIL);
    if (ECL_ARRAYP(val) && ecl_array_elttype(val) == ecl_aet_sf) {
        int nvalues = ecl_length(val);
        int offset = POPARG(as_int, 0);
        if (offset > nvalues) {
            offset = nvalues;
        }
        const float *values = (const float *)ecl_row_major_ptr(val, 0, 0);
        const char *overlay_text = POPARG(as_text, NULL);
        float scale_min = POPARG(as_float, FLT_MAX);
        float scale_max = POPARG(as_float, FLT_MAX);
        ImVec2 graph_size = POPARG(as_imvec2, ImVec2(0, 0));
        int stride = sizeof(float);
        ImGui::PlotLines(label, values, nvalues, offset, overlay_text, scale_min, scale_max, graph_size, stride);
    }
}
APIFUNC_END


APIFUNC(plothistogram)
{
    const char *label = POPARG(as_text, "label");
    cl_object val = POPARG(as_object, ECL_NIL);
    if (ECL_ARRAYP(val) && val->array.elttype == ecl_aet_sf) {
        int nvalues = ecl_length(val);
        int offset = POPARG(as_int, 0);
        if (offset > nvalues) {
            offset = nvalues;
        }
        const float *values = (const float *)ecl_row_major_ptr(val, 0, 0);
        const char *overlay_text = POPARG(as_text, NULL);
        float scale_min = POPARG(as_float, FLT_MAX);
        float scale_max = POPARG(as_float, FLT_MAX);
        ImVec2 graph_size = POPARG(as_imvec2, ImVec2(0, 0));
        int stride = sizeof(float);
        ImGui::PlotHistogram(label, values, nvalues, offset, overlay_text, scale_min, scale_max, graph_size, stride);
    }
}
APIFUNC_END

APIFUNC(image)
{
    unsigned int texture_id = POPARG(as_uint, 0U);
    if (texture_id) {
        ImVec2 size = POPARG(as_imvec2, ImVec2(0, 0));
        ImVec2 uv0 = POPARG(as_imvec2, ImVec2(0, 0));
        ImVec2 uv1 = POPARG(as_imvec2, ImVec2(1, 1));
        ImVec4 tint_col = POPARG(as_imvec4, ImVec4(1, 1, 1, 1));
        ImVec4 border_col = POPARG(as_imvec4, ImVec4(0, 0, 0, 0));
        ImGui::Image((void*)(intptr_t)texture_id, size, uv0, uv1, tint_col, border_col);
    }
}
APIFUNC_END

APIFUNC(imagebutton)
{
    bool ret = false;
    unsigned int texture_id = POPARG(as_uint, 0U);
    if (texture_id) {
        ImVec2 size = POPARG(as_imvec2, ImVec2(0, 0));
        ImVec2 uv0 = POPARG(as_imvec2, ImVec2(0, 0));
        ImVec2 uv1 = POPARG(as_imvec2, ImVec2(1, 1));
        int frame_padding = POPARG(as_int, -1);
        ImVec4 bg_col = POPARG(as_imvec4, ImVec4(0, 0, 0, 0));
        ImVec4 tint_col = POPARG(as_imvec4, ImVec4(1, 1, 1, 1));
        ret = ImGui::ImageButton((void*)(intptr_t)texture_id, size, uv0, uv1, frame_padding, bg_col, tint_col);
    }
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(loadtexture)
{
    const char *filename = POPARG(as_text, NULL);
    if (filename) {
        int width;
        int height;
        int nchannels;
        unsigned int texture = LoadTexture(filename, &width, &height, &nchannels);
        if (texture > 0) {
            // FIXME: hack to return 4 values without defining APIFUNC4
            cl_object result1 = ecl_make_uint32_t(texture);
            cl_object result2 = ecl_make_int32_t(width);
            cl_object result3 = ecl_make_int32_t(height);
            cl_object result4 = ecl_make_int32_t(nchannels);
            ecl_va_end(ap);
            // ... and there's no ecl_return4 ...
            cl_env_ptr env = ecl_process_env();
            ecl_nvalues(env) = 4;
            ecl_nth_value(env, 1) = result2;
            ecl_nth_value(env, 2) = result3;
            ecl_nth_value(env, 3) = result4;
            return result1;
        }
    }
}
APIFUNC_END

APIFUNC(createtexture)
{
    int width = POPARG(as_int, 0);
    int height = POPARG(as_int, 0);
    int nchannels = POPARG(as_int, 0);
    cl_object data = POPARG(as_object, ECL_NIL);
    if (width >= 0 && height >= 0) {
        if (nchannels >= 1 && nchannels <= 4) {
            if (ECL_ARRAYP(data) && ecl_array_elttype(data) == ecl_aet_b8) {
                int expected_size = width * height * nchannels;
                if (ecl_length(data) == expected_size) {
                    const void *ptr = ecl_row_major_ptr(data, 0, 0);
                    unsigned int texture = CreateTexture(width, height, nchannels, ptr);
                    RETUINT(texture);
                }
            }
        }
    }
}
APIFUNC_END

APIFUNC(deletetexture)
{
    unsigned int texture_id = POPARG(as_uint, 0U);
    if (texture_id) {
        DeleteTexture(texture_id);
    }
}
APIFUNC_END

APIFUNC(begindragdropsource)
{
    ImGuiDragDropFlags flags = POPARG(as_imguidragdropflags, ImGuiDragDropFlags_None);
    bool ret = ImGui::BeginDragDropSource(flags);
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(setdragdroppayload)
{
    cl_object object = POPARG(as_object, ECL_NIL);
    const char *type = "cl_object";
    const void *data = NULL;
    size_t sz = 0;
    ImGuiCond cond = POPARG(as_imguicond, ImGuiCond_Always);
    bool ret = ImGui::SetDragDropPayload(type, data, sz, cond);
    if (ret) {
        cl_set(intern("*IM-DRAG-DROP-PAYLOAD*"), object);
    }
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(enddragdropsource)
{
    ImGui::EndDragDropSource();
}
APIFUNC_END

APIFUNC(begindragdroptarget)
{
    bool ret = ImGui::BeginDragDropTarget();
    RETBOOL(ret);
}
APIFUNC_END

APIFUNC(acceptdragdroppayload)
{
    const char *type = "cl_object";
    ImGuiDragDropFlags flags = POPARG(as_imguidragdropflags, ImGuiDragDropFlags_None);
    const ImGuiPayload *payload = ImGui::AcceptDragDropPayload(type, flags);
    if (payload) {
        result = ecl_symbol_value(intern("*IM-DRAG-DROP-PAYLOAD*"));
    }
}
APIFUNC_END

APIFUNC(enddragdroptarget)
{
    ImGui::EndDragDropTarget();
}
APIFUNC_END

APIFUNC(getdragdroppayload)
{
    const ImGuiPayload *payload = ImGui::GetDragDropPayload();
    if (payload && payload->IsDataType("cl_object")) {
        result = ecl_symbol_value(intern("*IM-DRAG-DROP-PAYLOAD*"));
    }
}
APIFUNC_END

// Bindings definition

void imcl_define_bindings()
{
    define("BEGIN", clapi_begin);
    define("END", clapi_end);
    define("TEXT", clapi_text);
    define("%TEXT-COLORED", clapi_textcolored);
    define("TEXT-DISABLED", clapi_textdisabled);
    define("TEXT-WRAPPED", clapi_textwrapped);
    define("LABEL-TEXT", clapi_labeltext);
    define("BULLET-TEXT", clapi_bullettext);
    define("BUTTON", clapi_button);
    define("SAME-LINE", clapi_sameline);
    define("SEPARATOR", clapi_separator);
    define("BEGIN-GROUP", clapi_begingroup);
    define("END-GROUP", clapi_endgroup);
    define("PUSH-ITEM-WIDTH", clapi_pushitemwidth);
    define("POP-ITEM-WIDTH", clapi_popitemwidth);
    define("COLLAPSING-HEADER", clapi_collapsingheader);
    define("SPACING", clapi_spacing);
    define("TREE-NODE", clapi_treenode);
    define("TREE-POP", clapi_treepop);
    define("SET-NEXT-ITEM-WIDTH", clapi_setnextitemwidth);
    define("PUSH-STYLE-VAR", clapi_pushstylevar);
    define("POP-STYLE-VAR", clapi_popstylevar);
    define("COLUMNS", clapi_columns);
    define("PUSH-ID", clapi_pushid);
    define("POP-ID", clapi_popid);
    define("ALIGN-TEXT-TO-FRAME-PADDING", clapi_aligntexttoframepadding);
    define("NEXT-COLUMN", clapi_nextcolumn);
    define("SET-NEXT-WINDOW-SIZE", clapi_setnextwindowsize);
    define("BEGIN-CHILD", clapi_beginchild);
    define("END-CHILD", clapi_endchild);
    define("SELECTABLE", clapi_selectable);
    define("PUSH-STYLE-COLOR", clapi_pushstylecolor);
    define("POP-STYLE-COLOR", clapi_popstylecolor);
    define("SHOW-DEMO-WINDOW", clapi_showdemowindow);
    define("CHECKBOX", clapi_checkbox);
    define("RADIO-BUTTON", clapi_radiobutton);
    define("ITEM-HOVERED-P", clapi_isitemhovered);
    define("SET-TOOLTIP", clapi_settooltip);
    define("BEGIN-TOOLTIP", clapi_begintooltip);
    define("END-TOOLTIP", clapi_endtooltip);
    define("GET-CLIPBOARD-TEXT", clapi_getclipboardtext);
    define("SET-CLIPBOARD-TEXT", clapi_setclipboardtext);
    define("BEGIN-TAB-BAR", clapi_begintabbar);
    define("END-TAB-BAR", clapi_endtabbar);
    define("BEGIN-TAB-ITEM", clapi_begintabitem);
    define("END-TAB-ITEM", clapi_endtabitem);
    define("SET-TAB-ITEM-CLOSED", clapi_settabitemclosed);
    define("SET-ITEM-DEFAULT-FOCUS", clapi_setitemdefaultfocus);
    define("SET-KEYBOARD-FOCUS-HERE", clapi_setkeyboardfocushere);
    define("STYLE-COLORS", clapi_stylecolors);
    define("BEGIN-COMBO", clapi_begincombo);
    define("END-COMBO", clapi_endcombo);
    define("BEGIN-LISTBOX", clapi_beginlistbox);
    define("END-LISTBOX", clapi_endlistbox);
    define("BEGIN-MAIN-MENU-BAR", clapi_beginmainmenubar);
    define("END-MAIN-MENU-BAR", clapi_endmainmenubar);
    define("BEGIN-MENU-BAR", clapi_beginmenubar);
    define("END-MENU-BAR", clapi_endmenubar);
    define("BEGIN-MENU", clapi_beginmenu);
    define("END-MENU", clapi_endmenu);
    define("MENU-ITEM", clapi_menuitem);
    define("GET-SCROLL-X", clapi_getscrollx);
    define("GET-SCROLL-Y", clapi_getscrollx);
    define("GET-SCROLL-MAX-X", clapi_getscrollmaxx);
    define("GET-SCROLL-MAX-Y", clapi_getscrollmaxy);
    define("SET-SCROLL-X", clapi_setscrollx);
    define("SET-SCROLL-Y", clapi_setscrolly);
    define("SET-SCROLL-HERE-Y", clapi_setscrollherey);
    define("SET-SCROLL-FROM-POS-Y", clapi_setscrollfromposy);
    define("SLIDER-FLOAT", clapi_sliderfloat);
    define("SLIDER-ANGLE", clapi_sliderangle);
    define("SLIDER-INT", clapi_sliderint);
    define("VSLIDER-FLOAT", clapi_vsliderfloat);
    define("VSLIDER-INT", clapi_vsliderint);
    define("WINDOW-APPEARING-P", clapi_iswindowappearing);
    define("WINDOW-COLLAPSED-P", clapi_iswindowcollapsed);
    define("WINDOW-FOCUSED-P", clapi_iswindowfocused);
    define("WINDOW-HOVERED-P", clapi_iswindowhovered);
    define("GET-WINDOW-POS", clapi_getwindowpos);
    define("GET-WINDOW-SIZE", clapi_getwindowsize);
    define("SET-NEXT-WINDOW-POS", clapi_setnextwindowpos);
    define("SET-NEXT-WINDOW-CONTENT-SIZE", clapi_setnextwindowcontentsize);
    define("SET-NEXT-WINDOW-COLLAPSED", clapi_setnextwindowcollapsed);
    define("SET-NEXT-WINDOW-FOCUS", clapi_setnextwindowfocus);
    define("SET-NEXT-WINDOW-BG-ALPHA", clapi_setnextwindowbgalpha);
    define("SET-WINDOW-POS", clapi_setwindowpos);
    define("SET-WINDOW-SIZE", clapi_setwindowsize);
    define("SET-WINDOW-COLLAPSED", clapi_setwindowcollapsed);
    define("SET-WINDOW-FOCUS", clapi_setwindowfocus);
    define("SET-WINDOW-FONT-SCALE", clapi_setwindowfontscale);
    define("CALC-ITEM-WIDTH", clapi_calcitemwidth);
    define("PUSH-TEXT-WRAP-POS", clapi_pushtextwrappos);
    define("POP-TEXT-WRAP-POS", clapi_poptextwrappos);
    define("PUSH-ALLOW-KEYBOARD-FOCUS", clapi_pushallowkeyboardfocus);
    define("POP-ALLOW-KEYBOARD-FOCUS", clapi_popallowkeyboardfocus);
    define("PUSH-BUTTON-REPEAT", clapi_pushbuttonrepeat);
    define("POP-BUTTON-REPEAT", clapi_popbuttonrepeat);
    define("NEW-LINE", clapi_newline);
    define("DUMMY", clapi_dummy);
    define("INDENT", clapi_indent);
    define("UNINDENT", clapi_unindent);
    define("GET-CURSOR-POS", clapi_getcursorpos);
    define("SET-CURSOR-POS", clapi_setcursorpos);
    define("GET-CURSOR-START-POS", clapi_getcursorstartpos);
    define("GET-CURSOR-SCREEN-POS", clapi_getcursorscreenpos);
    define("SET-CURSOR-SCREEN-POS", clapi_setcursorscreenpos);
    define("GET-TEXT-LINE-HEIGHT", clapi_gettextlineheight);
    define("GET-TEXT-LINE-HEIGHT-WITH-SPACING", clapi_gettextlineheightwithspacing);
    define("GET-FRAME-HEIGHT", clapi_getframeheight);
    define("GET-FRAME-HEIGHT-WITH-SPACING", clapi_getframeheightwithspacing);
    define("SMALL-BUTTON", clapi_smallbutton);
    define("INVISIBLE-BUTTON", clapi_invisiblebutton);
    define("ARROW-BUTTON", clapi_arrowbutton);
    define("BULLET", clapi_bullet);
    define("GET-CONTENT-REGION-MAX", clapi_getcontentregionmax);
    define("GET-CONTENT-REGION-AVAIL", clapi_getcontentregionavail);
    define("GET-WINDOW-CONTENT-REGION-MIN", clapi_getwindowcontentregionmin);
    define("GET-WINDOW-CONTENT-REGION-MAX", clapi_getwindowcontentregionmax);
    define("PROGRESS-BAR", clapi_progressbar);
    define("ITEM-ACTIVE-P", clapi_isitemactive);
    define("ITEM-FOCUSED-P", clapi_isitemfocused);
    define("ITEM-CLICKED-P", clapi_isitemclicked);
    define("ITEM-VISIBLE-P", clapi_isitemvisible);
    define("ITEM-EDITED-P", clapi_isitemedited);
    define("ITEM-ACTIVATED-P", clapi_isitemactivated);
    define("ITEM-DEACTIVATED-P", clapi_isitemdeactivated);
    define("ITEM-DEACTIVATED-AFTER-EDIT-P", clapi_isitemdeactivatedafteredit);
    define("ANY-ITEM-HOVERED-P", clapi_isanyitemhovered);
    define("ANY-ITEM-ACTIVE-P", clapi_isanyitemactive);
    define("ANY-ITEM-FOCUSED-P", clapi_isanyitemfocused);
    define("DRAG-FLOAT", clapi_dragfloat);
    define("INPUT-FLOAT", clapi_inputfloat);
    define("DRAG-INT", clapi_dragint);
    define("INPUT-INT", clapi_inputint);
    define("GET-COLUMN-INDEX", clapi_getcolumnindex);
    define("GET-COLUMN-WIDTH", clapi_getcolumnwidth);
    define("SET-COLUMN-WIDTH", clapi_setcolumnwidth);
    define("GET-COLUMN-OFFSET", clapi_getcolumnoffset);
    define("SET-COLUMN-OFFSET", clapi_setcolumnoffset);
    define("GET-COLUMNS-COUNT", clapi_getcolumnscount);
    define("SHOW-ABOUT-WINDOW", clapi_showaboutwindow);
    define("SHOW-METRICS-WINDOW", clapi_showmetricswindow);
    define("SHOW-STYLE-EDITOR", clapi_showstyleeditor);
    define("SHOW-STYLE-SELECTOR", clapi_showstyleselector);
    define("SHOW-FONT-SELECTOR", clapi_showfontselector);
    define("SHOW-USER-GUIDE", clapi_showuserguide);
    define("IMGUI-VERSION", clapi_getversion);
    define("GET-ITEM-RECT-MIN", clapi_getitemrectmin);
    define("GET-ITEM-RECT-MAX", clapi_getitemrectmax);
    define("GET-ITEM-RECT-SIZE", clapi_getitemrectsize);
    define("SET-ITEM-ALLOW-OVERLAP", clapi_setitemallowoverlap);
    define("COLOR-EDIT", clapi_coloredit);
    define("COLOR-PICKER", clapi_colorpicker);
    define("COLOR-BUTTON", clapi_colorbutton);
    define("SET-COLOR-EDIT-OPTIONS", clapi_setcoloreditoptions);
    define("RECT-VISIBLE-P", clapi_isrectvisible);
    define("GET-TIME", clapi_gettime);
    define("GET-FRAME-COUNT", clapi_getframecount);
    define("CALC-TEXT-SIZE", clapi_calctextsize);
    define("CALC-LIST-CLIPPING", clapi_calclistclipping);
    define("TREE-PUSH", clapi_treepush);
    define("TREE-ADVANCE-TO-LABEL-POS", clapi_treeadvancetolabelpos);
    define("GET-TREE-NODE-TO-LABEL-SPACING", clapi_gettreenodetolabelspacing);
    define("SET-NEXT-TREE-NODE-OPEN", clapi_setnexttreenodeopen);
    define("OPEN-POPUP", clapi_openpopup);
    define("BEGIN-POPUP", clapi_beginpopup);
    define("BEGIN-POPUP-CONTEXT-ITEM", clapi_beginpopupcontextitem);
    define("BEGIN-POPUP-CONTEXT-WINDOW", clapi_beginpopupcontextwindow);
    define("BEGIN-POPUP-CONTEXT-VOID", clapi_beginpopupcontextvoid);
    define("BEGIN-POPUP-MODAL", clapi_beginpopupmodal);
    define("END-POPUP", clapi_endpopup);
    define("OPEN-POPUP-ON-ITEM-CLICK", clapi_openpopuponitemclick);
    define("POPUP-OPEN-P", clapi_ispopupopen);
    define("CLOSE-CURRENT-POPUP", clapi_closecurrentpopup);
    define("PUSH-CLIP-RECT", clapi_pushcliprect);
    define("POP-CLIP-RECT", clapi_popcliprect);
    define("GET-ID", clapi_getid);
    define("BEGIN-CHILD-FRAME", clapi_beginchildframe);
    define("END-CHILD-FRAME", clapi_endchildframe);
    define("RGB-TO-HSV", clapi_rgbtohsv);
    define("HSV-TO-RGB", clapi_hsvtorgb);
    define("GET-KEY-INDEX", clapi_getkeyindex);
    define("KEY-DOWN-P", clapi_iskeydown);
    define("KEY-PRESSED-P", clapi_iskeypressed);
    define("KEY-RELEASED-P", clapi_iskeyreleased);
    define("GET-KEY-PRESSED-AMOUNT", clapi_getkeypressedamount);
    define("MOUSE-DOWN-P", clapi_ismousedown);
    define("ANY-MOUSE-DOWN-P", clapi_isanymousedown);
    define("MOUSE-CLICKED-P", clapi_ismouseclicked);
    define("MOUSE-DOUBLE-CLICKED-P", clapi_ismousedoubleclicked);
    define("MOUSE-RELEASED-P", clapi_ismousereleased);
    define("MOUSE-DRAGGING-P", clapi_ismousedragging);
    define("MOUSE-HOVERING-RECT-P", clapi_ismousehoveringrect);
    define("MOUSE-POS-VALID-P", clapi_ismouseposvalid);
    define("GET-MOUSE-POS", clapi_getmousepos);
    define("GET-MOUSE-POS-ON-OPENING-CURRENT-POPUP", clapi_getmouseposonopeningcurrentpopup);
    define("GET-MOUSE-DRAG-DELTA", clapi_getmousedragdelta);
    define("RESET-MOUSE-DRAG-DELTA", clapi_resetmousedragdelta);
    define("GET-MOUSE-CURSOR", clapi_getmousecursor);
    define("SET-MOUSE-CURSOR", clapi_setmousecursor);
    define("CAPTURE-KEYBOARD-FROM-APP", clapi_capturekeyboardfromapp);
    define("CAPTURE-MOUSE-FROM-APP", clapi_capturemousefromapp);
    define("INPUT-TEXT", clapi_inputtext);
    define("INPUT-TEXT-MULTILINE", clapi_inputtextmultiline);
    define("INPUT-TEXT-WITH-HINT", clapi_inputtextwithhint);
    define("DRAG-FLOAT-RANGE", clapi_dragfloatrange);
    define("DRAG-INT-RANGE", clapi_dragintrange);
    define("PLOT-LINES", clapi_plotlines);
    define("PLOT-HISTOGRAM", clapi_plothistogram);
    define("IMAGE", clapi_image);
    define("IMAGE-BUTTON", clapi_imagebutton);
    define("BEGIN-DRAG-DROP-SOURCE", clapi_begindragdropsource);
    define("SET-DRAG-DROP-PAYLOAD", clapi_setdragdroppayload);
    define("END-DRAG-DROP-SOURCE", clapi_enddragdropsource);
    define("BEGIN-DRAG-DROP-TARGET", clapi_begindragdroptarget);
    define("ACCEPT-DRAG-DROP-PAYLOAD", clapi_acceptdragdroppayload);
    define("END-DRAG-DROP-TARGET", clapi_enddragdroptarget);
    define("GET-DRAG-DROP-PAYLOAD", clapi_getdragdroppayload);

    // Convenience functions not part of imgui
    define("LOAD-TEXTURE", clapi_loadtexture);
    define("CREATE-TEXTURE", clapi_createtexture);
    define("DELETE-TEXTURE", clapi_deletetexture);
}
