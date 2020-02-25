// dear imgui: standalone example application for GLFW + OpenGL 3, using programmable pipeline
// If you are new to dear imgui, see examples/README.txt and documentation at the top of imgui.cpp.
// (GLFW is a cross-platform general purpose library for handling windows, inputs, OpenGL/Vulkan graphics context creation, etc.)

#include "imgui.h"
#include "imgui_impl_glfw.h"
#include "imgui_impl_opengl3.h"
#include "cl_bindings.hpp"
#include <stdio.h>
#include <math.h>
#include <sstream>
#include <iostream>

// About OpenGL function loaders: modern OpenGL doesn't have a standard header file and requires individual function pointers to be loaded manually.
// Helper libraries are often used for this purpose! Here we are supporting a few common ones: gl3w, glew, glad.
// You may use another loader/header of your choice (glext, glLoadGen, etc.), or chose to manually implement your own.
#if defined(IMGUI_IMPL_OPENGL_LOADER_GL3W)
#include <GL/gl3w.h>    // Initialize with gl3wInit()
#elif defined(IMGUI_IMPL_OPENGL_LOADER_GLEW)
#include <GL/glew.h>    // Initialize with glewInit()
#elif defined(IMGUI_IMPL_OPENGL_LOADER_GLAD)
#include <glad/glad.h>  // Initialize with gladLoadGL()
#else
#include IMGUI_IMPL_OPENGL_LOADER_CUSTOM
#endif

// Include glfw3.h after our OpenGL definitions
#include <GLFW/glfw3.h>

// [Win32] Our example includes a copy of glfw3.lib pre-compiled with VS2010 to maximize ease of testing and compatibility with old VS compilers.
// To link with VS2010-era libraries, VS2015+ requires linking with legacy_stdio_definitions.lib, which we do using this pragma.
// Your own project should not be affected, as you are likely to link with a newer binary of GLFW that is adequate for your version of Visual Studio.
#if defined(_MSC_VER) && (_MSC_VER >= 1900) && !defined(IMGUI_DISABLE_WIN32_FUNCTIONS)
#pragma comment(lib, "legacy_stdio_definitions")
#endif

static void glfw_error_callback(int error, const char* description)
{
    fprintf(stderr, "Glfw Error %d: %s\n", error, description);
}

enum
{
    ERROR_STATE_MESSAGE_NCHARS = 512
};

struct error_state
{
    int show_error;
    char message[ERROR_STATE_MESSAGE_NCHARS];
};

struct error_state g_error_state;

void imgui_ecl_assert_fail(const char *assertion,
                           const char *file,
                           unsigned int line,
                           const char *function)
{
    g_error_state.show_error = 1;
    snprintf(g_error_state.message, ERROR_STATE_MESSAGE_NCHARS,
             "Assertion failure in '%s' (%s:%d):\n%s",
             function, file, line, assertion);
}

int main(int argc, char **argv)
{
    cl_boot(argc, argv);

    // Setup window
    glfwSetErrorCallback(glfw_error_callback);
    if (!glfwInit())
        return 1;

    // Decide GL+GLSL versions
#if __APPLE__
    // GL 3.2 + GLSL 150
    const char* glsl_version = "#version 150";
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 2);
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);  // 3.2+ only
    glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);            // Required on Mac
#else
    // GL 3.0 + GLSL 130
    const char* glsl_version = "#version 130";
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 0);
    //glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);  // 3.2+ only
    //glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);            // 3.0+ only
#endif

    // Create window with graphics context
    GLFWwindow* window = glfwCreateWindow(1280, 720, "Dear ImGui GLFW+OpenGL3 example", NULL, NULL);
    if (window == NULL)
        return 1;
    glfwMakeContextCurrent(window);
    glfwSwapInterval(1); // Enable vsync

    // Initialize OpenGL loader
#if defined(IMGUI_IMPL_OPENGL_LOADER_GL3W)
    bool err = gl3wInit() != 0;
#elif defined(IMGUI_IMPL_OPENGL_LOADER_GLEW)
    bool err = glewInit() != GLEW_OK;
#elif defined(IMGUI_IMPL_OPENGL_LOADER_GLAD)
    bool err = gladLoadGL() == 0;
#else
    bool err = false; // If you use IMGUI_IMPL_OPENGL_LOADER_CUSTOM, your loader is likely to requires some form of initialization.
#endif
    if (err)
    {
        fprintf(stderr, "Failed to initialize OpenGL loader!\n");
        return 1;
    }

    // Setup Dear ImGui context
    IMGUI_CHECKVERSION();
    ImGui::CreateContext();
    ImGuiIO& io = ImGui::GetIO(); (void)io;
    //io.ConfigFlags |= ImGuiConfigFlags_NavEnableKeyboard;  // Enable Keyboard Controls
    //io.ConfigFlags |= ImGuiConfigFlags_NavEnableGamepad;   // Enable Gamepad Controls

    // Setup Dear ImGui style
    ImGui::StyleColorsDark();
    //ImGui::StyleColorsClassic();

    // Setup Platform/Renderer bindings
    ImGui_ImplGlfw_InitForOpenGL(window, true);
    ImGui_ImplOpenGL3_Init(glsl_version);

    // Load Fonts
    // - If no fonts are loaded, dear imgui will use the default font. You can also load multiple fonts and use ImGui::PushFont()/PopFont() to select them.
    // - AddFontFromFileTTF() will return the ImFont* so you can store it if you need to select the font among multiple.
    // - If the file cannot be loaded, the function will return NULL. Please handle those errors in your application (e.g. use an assertion, or display an error and quit).
    // - The fonts will be rasterized at a given size (w/ oversampling) and stored into a texture when calling ImFontAtlas::Build()/GetTexDataAsXXXX(), which ImGui_ImplXXXX_NewFrame below will call.
    // - Read 'misc/fonts/README.txt' for more instructions and details.
    // - Remember that in C/C++ if you want to include a backslash \ in a string literal you need to write a double backslash \\ !
    //io.Fonts->AddFontDefault();
    //io.Fonts->AddFontFromFileTTF("../../misc/fonts/Roboto-Medium.ttf", 16.0f);
    //io.Fonts->AddFontFromFileTTF("../../misc/fonts/Cousine-Regular.ttf", 15.0f);
    //io.Fonts->AddFontFromFileTTF("../../misc/fonts/DroidSans.ttf", 16.0f);
    //io.Fonts->AddFontFromFileTTF("../../misc/fonts/ProggyTiny.ttf", 10.0f);
    //ImFont* font = io.Fonts->AddFontFromFileTTF("c:\\Windows\\Fonts\\ArialUni.ttf", 18.0f, NULL, io.Fonts->GetGlyphRangesJapanese());
    //IM_ASSERT(font != NULL);

    ImVec4 clear_color = ImVec4(0.45f, 0.55f, 0.60f, 1.00f);

    cl_define_bindings();

    cl_object window_cl = ecl_make_pointer(window);
    cl_set(imgui_intern_and_export("*GLFW-WINDOW*"), window_cl);

    cl_object time_in_imtick_var = imgui_intern_and_export("*TIME-IN-IM-TICK*");
    cl_set(time_in_imtick_var, ecl_make_int32_t(0));

    cl_object time_in_gltick_var = imgui_intern_and_export("*TIME-IN-GL-TICK*");
    cl_set(time_in_gltick_var, ecl_make_int32_t(0));

    cl_eval(c_string_to_object("(load \"main\")"));
    cl_eval(c_string_to_object("(init)"));

    cl_object callgltick = c_string_to_object("(gl-tick)");
    cl_object callimtick = c_string_to_object("(im-tick)");

    g_error_state.show_error = 0;
    g_error_state.message[0] = '\0';

    // Main loop
    while (!glfwWindowShouldClose(window))
    {
        // Poll and handle events (inputs, window resize, etc.)
        //
        // You can read the io.WantCaptureMouse,
        // io.WantCaptureKeyboard flags to tell if dear imgui wants to
        // use your inputs.
        //
        // - When io.WantCaptureMouse is true, do not dispatch mouse
        //   input data to your main application.
        //
        // - When io.WantCaptureKeyboard is true, do not dispatch
        //   keyboard input data to your main application.
        //
        // Generally you may always pass all inputs to dear imgui, and
        // hide them from your application based on those two flags.
        glfwPollEvents();

        // Start the Dear ImGui frame
        ImGui_ImplOpenGL3_NewFrame();
        ImGui_ImplGlfw_NewFrame();
        ImGui::NewFrame();

        if (!g_error_state.show_error) {
            double lisp_start_time = glfwGetTime();
            cl_env_ptr env = ecl_process_env();
            ECL_CATCH_ALL_BEGIN(env) {
                cl_eval(callimtick);
            }
            ECL_CATCH_ALL_IF_CAUGHT {
                g_error_state.show_error = 1;
                snprintf(g_error_state.message, ERROR_STATE_MESSAGE_NCHARS,
                         "An error was signaled by IM-TICK.");
            }
            ECL_CATCH_ALL_END;
            double lisp_end_time = glfwGetTime();
            double lisp_duration = lisp_end_time - lisp_start_time;
            int time_in_lisp = (int)(lisp_duration * 1000000.0);
            cl_set(time_in_imtick_var, ecl_make_int32_t(time_in_lisp));
        }

        if (g_error_state.show_error) {
            if (ImGui::Begin("Error")) {
                ImGui::Text("%s", g_error_state.message);
                if (ImGui::Button("Retry")) {
                    g_error_state.show_error = 0;
                    g_error_state.message[0] = '\0';
                }
            }
            ImGui::End();
        }

        // Rendering
        ImGui::Render();
        int display_w, display_h;
        glfwMakeContextCurrent(window);
        glfwGetFramebufferSize(window, &display_w, &display_h);
        glViewport(0, 0, display_w, display_h);

        // Only needed if we don't do it on the Lisp side...
        glClearColor(clear_color.x, clear_color.y, clear_color.z, clear_color.w);
        glClear(GL_COLOR_BUFFER_BIT);

        if (!g_error_state.show_error) {
            double lisp_start_time = glfwGetTime();
            cl_env_ptr env = ecl_process_env();
            ECL_CATCH_ALL_BEGIN(env) {
                cl_eval(callgltick);
            }
            ECL_CATCH_ALL_IF_CAUGHT {
                g_error_state.show_error = 1;
                snprintf(g_error_state.message, ERROR_STATE_MESSAGE_NCHARS,
                         "An error was signaled by GL-TICK.");
            }
            ECL_CATCH_ALL_END;
            double lisp_end_time = glfwGetTime();
            double lisp_duration = lisp_end_time - lisp_start_time;
            int time_in_lisp = (int)(lisp_duration * 1000000.0);
            cl_set(time_in_gltick_var, ecl_make_int32_t(time_in_lisp));
        }

        ImGui_ImplOpenGL3_RenderDrawData(ImGui::GetDrawData());

        glfwMakeContextCurrent(window);
        glfwSwapBuffers(window);
    }

    // Cleanup
    ImGui_ImplOpenGL3_Shutdown();
    ImGui_ImplGlfw_Shutdown();
    ImGui::DestroyContext();

    glfwDestroyWindow(window);
    glfwTerminate();

    cl_shutdown();

    return 0;
}
