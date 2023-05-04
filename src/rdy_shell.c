#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <reflect.h>

#include "libdyibicc.h"
#include "raylib.h"

static DyibiccContext* cc_ctx;
static bool last_compile_successful;
static void* connection_handle;

extern bool nvim_connection_setup(const char* files[],
                                  const char* nvim_config_fullpath,
                                  void** connection_handle);
extern bool nvim_connection_poll(void (*file_update)(char* name, char* contents));
extern bool nvim_connection_send_quit_and_shutdown(void* connection_handle);
extern bool nvim_connection_send_extmark_update(void* pipe, char* file, int line, char* contents);
extern bool nvim_connection_send_clear_all_extmarks(void* pipe);

extern void console_preinit(void);
extern void console_init(void);
extern void console_update(void);

extern void console_main_set_visible(bool visible);
extern void console_error_set_visible(bool visible);

extern int console_main_out(const char* text);
extern void console_main_printf(const char* fmt, ...);
extern int console_main_vprintf(const char* fmt, va_list ap);
extern void console_main_clear(void);

extern int console_error_out(const char* text);
extern void console_error_printf(const char* fmt, ...);
extern int console_error_vprintf(const char* fmt, va_list ap);

extern void console_error_clear(void);

extern void console_shutdown(void);

static int output_function(const char* fmt, va_list ap) {
  return console_error_vprintf(fmt, ap);
}

static void Log(const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  console_main_vprintf(fmt, ap);
}

static void qq_fmt(char* into, void* data, _ReflectType* type) {
  char tmp[256];
  if (type->kind == _REFLECT_KIND_INT) {
    sprintf(tmp, "%d", *(int*)data);
    strcat(into, tmp);
  } else if (type->kind == _REFLECT_KIND_BOOL) {
      sprintf(tmp, "%s", *(bool*)data ? "true" : "false");
      strcat(into, tmp);
  } else if (type->kind == _REFLECT_KIND_FLOAT) {
    sprintf(tmp, "%f", *(float*)data);
    strcat(into, tmp);
  } else if (type->kind == _REFLECT_KIND_DOUBLE) {
    sprintf(tmp, "%f", *(double*)data);
    strcat(into, tmp);
  } else {
    strcat(into, "TODO");
  }
}

static void qq_eval(char* file, int line, _ReflectType* type, ...) {
  char buf[1024];

  va_list ap;
  va_start(ap, type);
  sprintf(buf, "= ");
  if (type->kind == _REFLECT_KIND_INT) {
    int x = va_arg(ap, int);
    qq_fmt(buf, &x, type);
  } else if (type->kind == _REFLECT_KIND_BOOL) {
    // '...' passes as int, even if a bool is specified.
    bool x = (bool)va_arg(ap, int);
    qq_fmt(buf, &x, type);
  } else if (type->kind == _REFLECT_KIND_FLOAT) {
    // '...' passes as double, even if a float is specified.
    float x = (float)va_arg(ap, double);
    qq_fmt(buf, &x, type);
  } else if (type->kind == _REFLECT_KIND_DOUBLE) {
    double x = va_arg(ap, double);
    qq_fmt(buf, &x, type);
  } else if (type->kind == _REFLECT_KIND_STRUCT) {
    strcat(buf, "{");
    char* p = va_arg(ap, char*);
    for (size_t i = 0; i < type->su.num_members; ++i) {
      strcat(buf, type->su.members[i].name);
      strcat(buf, "=");
      qq_fmt(buf, (p + type->su.members[i].offset), type->su.members[i].type);
      if (i < type->su.num_members - 1)
        strcat(buf, ",");
    }
    strcat(buf, "}");
  } else {
    sprintf(buf, "...todo");
  }
  strcat(buf, " (");
  strcat(buf, type->name);
  strcat(buf, ")");
  va_end(ap);

  nvim_connection_send_extmark_update(connection_handle, file, line - 1, buf);
}

void custom_raylib_log(int log_type, const char *fmt, va_list ap) {
  if (log_type < LOG_WARNING) {
    console_main_out("\033[0m"); // Reset
  } else if (log_type == LOG_WARNING) {
    console_main_out("\033[1;35m"); // Violet
  } else if (log_type >= LOG_ERROR) {
    console_main_out("\033[1;31m"); // Red
  }
  console_main_vprintf(fmt, ap);
  console_main_out("\033[0m\n");
}

static void* provide_function(const char* name) {
#define X(n)                 \
  if (strcmp(#n, name) == 0) \
    return (void*)n;
  X(Log);
  X(qq_eval);

  X(InitWindow);
  X(WindowShouldClose);
  X(CloseWindow);
  X(IsWindowReady);
  X(IsWindowFullscreen);
  X(IsWindowHidden);
  X(IsWindowMinimized);
  X(IsWindowMaximized);
  X(IsWindowFocused);
  X(IsWindowResized);
  X(IsWindowState);
  X(SetWindowState);
  X(ClearWindowState);
  X(ToggleFullscreen);
  X(MaximizeWindow);
  X(MinimizeWindow);
  X(RestoreWindow);
  X(SetWindowIcon);
  X(SetWindowIcons);
  X(SetWindowTitle);
  X(SetWindowPosition);
  X(SetWindowMonitor);
  X(SetWindowMinSize);
  X(SetWindowSize);
  X(SetWindowOpacity);
  X(GetWindowHandle);
  X(GetScreenWidth);
  X(GetScreenHeight);
  X(GetRenderWidth);
  X(GetRenderHeight);
  X(GetMonitorCount);
  X(GetCurrentMonitor);
  X(GetMonitorPosition);
  X(GetMonitorWidth);
  X(GetMonitorHeight);
  X(GetMonitorPhysicalWidth);
  X(GetMonitorPhysicalHeight);
  X(GetMonitorRefreshRate);
  X(GetWindowPosition);
  X(GetWindowScaleDPI);
  X(GetMonitorName);
  X(SetClipboardText);
  X(GetClipboardText);
  X(EnableEventWaiting);
  X(DisableEventWaiting);
  X(SwapScreenBuffer);
  X(PollInputEvents);
  X(WaitTime);
  X(ShowCursor);
  X(HideCursor);
  X(IsCursorHidden);
  X(EnableCursor);
  X(DisableCursor);
  X(IsCursorOnScreen);
  X(ClearBackground);
  X(BeginDrawing);
  X(EndDrawing);
  X(BeginMode2D);
  X(EndMode2D);
  X(BeginMode3D);
  X(EndMode3D);
  X(BeginTextureMode);
  X(EndTextureMode);
  X(BeginShaderMode);
  X(EndShaderMode);
  X(BeginBlendMode);
  X(EndBlendMode);
  X(BeginScissorMode);
  X(EndScissorMode);
  X(BeginVrStereoMode);
  X(EndVrStereoMode);
  X(LoadVrStereoConfig);
  X(UnloadVrStereoConfig);
  X(LoadShader);
  X(LoadShaderFromMemory);
  X(IsShaderReady);
  X(GetShaderLocation);
  X(GetShaderLocationAttrib);
  X(SetShaderValue);
  X(SetShaderValueV);
  X(SetShaderValueMatrix);
  X(SetShaderValueTexture);
  X(UnloadShader);
  X(GetMouseRay);
  X(GetCameraMatrix);
  X(GetCameraMatrix2D);
  X(GetWorldToScreen);
  X(GetScreenToWorld2D);
  X(GetWorldToScreenEx);
  X(GetWorldToScreen2D);
  X(SetTargetFPS);
  X(GetFPS);
  X(GetFrameTime);
  X(GetTime);
  X(GetRandomValue);
  X(SetRandomSeed);
  X(TakeScreenshot);
  X(SetConfigFlags);
  X(TraceLog);
  X(SetTraceLogLevel);
  X(MemAlloc);
  X(MemRealloc);
  X(MemFree);
  X(OpenURL);
  X(SetTraceLogCallback);
  X(SetLoadFileDataCallback);
  X(SetSaveFileDataCallback);
  X(SetLoadFileTextCallback);
  X(SetSaveFileTextCallback);
  X(LoadFileData);
  X(UnloadFileData);
  X(SaveFileData);
  X(ExportDataAsCode);
  X(LoadFileText);
  X(UnloadFileText);
  X(SaveFileText);
  X(FileExists);
  X(DirectoryExists);
  X(IsFileExtension);
  X(GetFileLength);
  X(GetFileExtension);
  X(GetFileName);
  X(GetFileNameWithoutExt);
  X(GetDirectoryPath);
  X(GetPrevDirectoryPath);
  X(GetWorkingDirectory);
  X(GetApplicationDirectory);
  X(ChangeDirectory);
  X(IsPathFile);
  X(LoadDirectoryFiles);
  X(LoadDirectoryFilesEx);
  X(UnloadDirectoryFiles);
  X(IsFileDropped);
  X(LoadDroppedFiles);
  X(UnloadDroppedFiles);
  X(GetFileModTime);
  X(CompressData);
  X(DecompressData);
  X(EncodeDataBase64);
  X(DecodeDataBase64);
  X(IsKeyPressed);
  X(IsKeyDown);
  X(IsKeyReleased);
  X(IsKeyUp);
  X(SetExitKey);
  X(GetKeyPressed);
  X(GetCharPressed);
  X(IsGamepadAvailable);
  X(GetGamepadName);
  X(IsGamepadButtonPressed);
  X(IsGamepadButtonDown);
  X(IsGamepadButtonReleased);
  X(IsGamepadButtonUp);
  X(GetGamepadButtonPressed);
  X(GetGamepadAxisCount);
  X(GetGamepadAxisMovement);
  X(SetGamepadMappings);
  X(IsMouseButtonPressed);
  X(IsMouseButtonDown);
  X(IsMouseButtonReleased);
  X(IsMouseButtonUp);
  X(GetMouseX);
  X(GetMouseY);
  X(GetMousePosition);
  X(GetMouseDelta);
  X(SetMousePosition);
  X(SetMouseOffset);
  X(SetMouseScale);
  X(GetMouseWheelMove);
  X(GetMouseWheelMoveV);
  X(SetMouseCursor);
  X(GetTouchX);
  X(GetTouchY);
  X(GetTouchPosition);
  X(GetTouchPointId);
  X(GetTouchPointCount);
  X(SetGesturesEnabled);
  X(IsGestureDetected);
  X(GetGestureDetected);
  X(GetGestureHoldDuration);
  X(GetGestureDragVector);
  X(GetGestureDragAngle);
  X(GetGesturePinchVector);
  X(GetGesturePinchAngle);
  X(UpdateCamera);
  X(UpdateCameraPro);
  X(SetShapesTexture);
  X(DrawPixel);
  X(DrawPixelV);
  X(DrawLine);
  X(DrawLineV);
  X(DrawLineEx);
  X(DrawLineBezier);
  X(DrawLineBezierQuad);
  X(DrawLineBezierCubic);
  X(DrawLineStrip);
  X(DrawCircle);
  X(DrawCircleSector);
  X(DrawCircleSectorLines);
  X(DrawCircleGradient);
  X(DrawCircleV);
  X(DrawCircleLines);
  X(DrawEllipse);
  X(DrawEllipseLines);
  X(DrawRing);
  X(DrawRingLines);
  X(DrawRectangle);
  X(DrawRectangleV);
  X(DrawRectangleRec);
  X(DrawRectanglePro);
  X(DrawRectangleGradientV);
  X(DrawRectangleGradientH);
  X(DrawRectangleGradientEx);
  X(DrawRectangleLines);
  X(DrawRectangleLinesEx);
  X(DrawRectangleRounded);
  X(DrawRectangleRoundedLines);
  X(DrawTriangle);
  X(DrawTriangleLines);
  X(DrawTriangleFan);
  X(DrawTriangleStrip);
  X(DrawPoly);
  X(DrawPolyLines);
  X(DrawPolyLinesEx);
  X(CheckCollisionRecs);
  X(CheckCollisionCircles);
  X(CheckCollisionCircleRec);
  X(CheckCollisionPointRec);
  X(CheckCollisionPointCircle);
  X(CheckCollisionPointTriangle);
  X(CheckCollisionPointPoly);
  X(CheckCollisionLines);
  X(CheckCollisionPointLine);
  X(GetCollisionRec);
  X(LoadImage);
  X(LoadImageRaw);
  X(LoadImageAnim);
  X(LoadImageFromMemory);
  X(LoadImageFromTexture);
  X(LoadImageFromScreen);
  X(IsImageReady);
  X(UnloadImage);
  X(ExportImage);
  X(ExportImageAsCode);
  X(GenImageColor);
  X(GenImageGradientV);
  X(GenImageGradientH);
  X(GenImageGradientRadial);
  X(GenImageChecked);
  X(GenImageWhiteNoise);
  X(GenImagePerlinNoise);
  X(GenImageCellular);
  X(GenImageText);
  X(ImageCopy);
  X(ImageFromImage);
  X(ImageText);
  X(ImageTextEx);
  X(ImageFormat);
  X(ImageToPOT);
  X(ImageCrop);
  X(ImageAlphaCrop);
  X(ImageAlphaClear);
  X(ImageAlphaMask);
  X(ImageAlphaPremultiply);
  X(ImageBlurGaussian);
  X(ImageResize);
  X(ImageResizeNN);
  X(ImageResizeCanvas);
  X(ImageMipmaps);
  X(ImageDither);
  X(ImageFlipVertical);
  X(ImageFlipHorizontal);
  X(ImageRotateCW);
  X(ImageRotateCCW);
  X(ImageColorTint);
  X(ImageColorInvert);
  X(ImageColorGrayscale);
  X(ImageColorContrast);
  X(ImageColorBrightness);
  X(ImageColorReplace);
  X(LoadImageColors);
  X(LoadImagePalette);
  X(UnloadImageColors);
  X(UnloadImagePalette);
  X(GetImageAlphaBorder);
  X(GetImageColor);
  X(ImageClearBackground);
  X(ImageDrawPixel);
  X(ImageDrawPixelV);
  X(ImageDrawLine);
  X(ImageDrawLineV);
  X(ImageDrawCircle);
  X(ImageDrawCircleV);
  X(ImageDrawCircleLines);
  X(ImageDrawCircleLinesV);
  X(ImageDrawRectangle);
  X(ImageDrawRectangleV);
  X(ImageDrawRectangleRec);
  X(ImageDrawRectangleLines);
  X(ImageDraw);
  X(ImageDrawText);
  X(ImageDrawTextEx);
  X(LoadTexture);
  X(LoadTextureFromImage);
  X(LoadTextureCubemap);
  X(LoadRenderTexture);
  X(IsTextureReady);
  X(UnloadTexture);
  X(IsRenderTextureReady);
  X(UnloadRenderTexture);
  X(UpdateTexture);
  X(UpdateTextureRec);
  X(GenTextureMipmaps);
  X(SetTextureFilter);
  X(SetTextureWrap);
  X(DrawTexture);
  X(DrawTextureV);
  X(DrawTextureEx);
  X(DrawTextureRec);
  X(DrawTexturePro);
  X(DrawTextureNPatch);
  X(Fade);
  X(ColorToInt);
  X(ColorNormalize);
  X(ColorFromNormalized);
  X(ColorToHSV);
  X(ColorFromHSV);
  X(ColorTint);
  X(ColorBrightness);
  X(ColorContrast);
  X(ColorAlpha);
  X(ColorAlphaBlend);
  X(GetColor);
  X(GetPixelColor);
  X(SetPixelColor);
  X(GetPixelDataSize);
  X(GetFontDefault);
  X(LoadFont);
  X(LoadFontEx);
  X(LoadFontFromImage);
  X(LoadFontFromMemory);
  X(IsFontReady);
  X(LoadFontData);
  X(GenImageFontAtlas);
  X(UnloadFontData);
  X(UnloadFont);
  X(ExportFontAsCode);
  X(DrawFPS);
  X(DrawText);
  X(DrawTextEx);
  X(DrawTextPro);
  X(DrawTextCodepoint);
  X(DrawTextCodepoints);
  X(MeasureText);
  X(MeasureTextEx);
  X(GetGlyphIndex);
  X(GetGlyphInfo);
  X(GetGlyphAtlasRec);
  X(LoadUTF8);
  X(UnloadUTF8);
  X(LoadCodepoints);
  X(UnloadCodepoints);
  X(GetCodepointCount);
  X(GetCodepoint);
  X(GetCodepointNext);
  X(GetCodepointPrevious);
  X(CodepointToUTF8);
  X(TextCopy);
  X(TextIsEqual);
  X(TextLength);
  X(TextFormat);
  X(TextSubtext);
  X(TextReplace);
  X(TextInsert);
  X(TextJoin);
  X(TextSplit);
  X(TextAppend);
  X(TextFindIndex);
  X(TextToUpper);
  X(TextToLower);
  X(TextToPascal);
  X(TextToInteger);
  X(DrawLine3D);
  X(DrawPoint3D);
  X(DrawCircle3D);
  X(DrawTriangle3D);
  X(DrawTriangleStrip3D);
  X(DrawCube);
  X(DrawCubeV);
  X(DrawCubeWires);
  X(DrawCubeWiresV);
  X(DrawSphere);
  X(DrawSphereEx);
  X(DrawSphereWires);
  X(DrawCylinder);
  X(DrawCylinderEx);
  X(DrawCylinderWires);
  X(DrawCylinderWiresEx);
  X(DrawCapsule);
  X(DrawCapsuleWires);
  X(DrawPlane);
  X(DrawRay);
  X(DrawGrid);
  X(LoadModel);
  X(LoadModelFromMesh);
  X(IsModelReady);
  X(UnloadModel);
  X(GetModelBoundingBox);
  X(DrawModel);
  X(DrawModelEx);
  X(DrawModelWires);
  X(DrawModelWiresEx);
  X(DrawBoundingBox);
  X(DrawBillboard);
  X(DrawBillboardRec);
  X(DrawBillboardPro);
  X(UploadMesh);
  X(UpdateMeshBuffer);
  X(UnloadMesh);
  X(DrawMesh);
  X(DrawMeshInstanced);
  X(ExportMesh);
  X(GetMeshBoundingBox);
  X(GenMeshTangents);
  X(GenMeshPoly);
  X(GenMeshPlane);
  X(GenMeshCube);
  X(GenMeshSphere);
  X(GenMeshHemiSphere);
  X(GenMeshCylinder);
  X(GenMeshCone);
  X(GenMeshTorus);
  X(GenMeshKnot);
  X(GenMeshHeightmap);
  X(GenMeshCubicmap);
  X(LoadMaterials);
  X(LoadMaterialDefault);
  X(IsMaterialReady);
  X(UnloadMaterial);
  X(SetMaterialTexture);
  X(SetModelMeshMaterial);
  X(LoadModelAnimations);
  X(UpdateModelAnimation);
  X(UnloadModelAnimation);
  X(UnloadModelAnimations);
  X(IsModelAnimationValid);
  X(CheckCollisionSpheres);
  X(CheckCollisionBoxes);
  X(CheckCollisionBoxSphere);
  X(GetRayCollisionSphere);
  X(GetRayCollisionBox);
  X(GetRayCollisionMesh);
  X(GetRayCollisionTriangle);
  X(GetRayCollisionQuad);
  X(InitAudioDevice);
  X(CloseAudioDevice);
  X(IsAudioDeviceReady);
  X(SetMasterVolume);
  X(LoadWave);
  X(LoadWaveFromMemory);
  X(IsWaveReady);
  X(LoadSound);
  X(LoadSoundFromWave);
  X(IsSoundReady);
  X(UpdateSound);
  X(UnloadWave);
  X(UnloadSound);
  X(ExportWave);
  X(ExportWaveAsCode);
  X(PlaySound);
  X(StopSound);
  X(PauseSound);
  X(ResumeSound);
  X(IsSoundPlaying);
  X(SetSoundVolume);
  X(SetSoundPitch);
  X(SetSoundPan);
  X(WaveCopy);
  X(WaveCrop);
  X(WaveFormat);
  X(LoadWaveSamples);
  X(UnloadWaveSamples);
  X(LoadMusicStream);
  X(LoadMusicStreamFromMemory);
  X(IsMusicReady);
  X(UnloadMusicStream);
  X(PlayMusicStream);
  X(IsMusicStreamPlaying);
  X(UpdateMusicStream);
  X(StopMusicStream);
  X(PauseMusicStream);
  X(ResumeMusicStream);
  X(SeekMusicStream);
  X(SetMusicVolume);
  X(SetMusicPitch);
  X(SetMusicPan);
  X(GetMusicTimeLength);
  X(GetMusicTimePlayed);
  X(LoadAudioStream);
  X(IsAudioStreamReady);
  X(UnloadAudioStream);
  X(UpdateAudioStream);
  X(IsAudioStreamProcessed);
  X(PlayAudioStream);
  X(PauseAudioStream);
  X(ResumeAudioStream);
  X(IsAudioStreamPlaying);
  X(StopAudioStream);
  X(SetAudioStreamVolume);
  X(SetAudioStreamPitch);
  X(SetAudioStreamPan);
  X(SetAudioStreamBufferSizeDefault);
  X(SetAudioStreamCallback);
  X(AttachAudioStreamProcessor);
  X(DetachAudioStreamProcessor);
  X(AttachAudioMixedProcessor);
  X(DetachAudioMixedProcessor);
#undef X
  return NULL;
}

// Can't include windows.h here because of raylib conflicts.
extern unsigned int GetFullPathNameA(const char* lpFileName,
                                     unsigned int nBufferLength,
                                     char* lpBuffer,
                                     char** lpFilePart);

static char* fullpath(const char* relpath) {
  char buf[1024];
  GetFullPathNameA(relpath, sizeof(buf), buf, NULL);
  return _strdup(buf);
}

typedef struct PendingUpdate {
  char* filename;
  char* contents;
} PendingUpdate;
static PendingUpdate* pending_updates;
static int num_pending_files;

static void allocate_pending_updates(const char* files[]) {
  num_pending_files = 0;
  for (char**p = files; *p; ++p) {
    ++num_pending_files;
  }

  pending_updates = calloc(num_pending_files, sizeof(PendingUpdate));
  int i = 0;
  for (char**p = files; *p; ++p) {
    PendingUpdate* pu = &pending_updates[i++];
    pu->filename = _strdup(*p);
  }
}

static void free_pending_updates(void) {
  for (int i = 0; i < num_pending_files; ++i) {
    PendingUpdate* pu = &pending_updates[i];
    free(pu->filename);
    if (pu->contents) {
      free(pu->contents);
      pu->contents = NULL;
    }
  }
}

static void file_update_notification(char* filename, char* contents) {
  for (int i = 0; i < num_pending_files; ++i) {
    PendingUpdate* pu = &pending_updates[i];
    if (strcmp(pu->filename, filename) == 0) {
      if (pu->contents) {
        free(pu->contents);
      }
      pu->contents = _strdup(contents);
      return;
    }
  }
  console_main_printf("'%s' got update but wasn't in project?", filename);
}

static void commit_last_change_for_each_file(void) {
  bool first_this_frame = true;
  // If multiple updates arrived during one RPC poll, we only bother compiling
  // the last one. This helps avoid getting "behind" during rapid changes in the
  // editor, like a long autocomplete or rapid undo/redo.
  for (int i = 0; i < num_pending_files; ++i) {
    PendingUpdate* pu = &pending_updates[i];
    if (pu->contents) {
      if (first_this_frame) {
        console_error_clear();
        last_compile_successful = true;
        first_this_frame = false;
      }

      nvim_connection_send_clear_all_extmarks(connection_handle);
      if (!dyibicc_update(cc_ctx, pu->filename, pu->contents)) {
        last_compile_successful = false;
        // Don't break here, otherwise an update with multiple files would lose
        // the later update if an earlier one had a compile error.
      }
      free(pu->contents);
      pu->contents = NULL;
    }
  }
}

int main(void) {
  console_preinit();
  SetTraceLogCallback(custom_raylib_log);

  SetConfigFlags(FLAG_VSYNC_HINT);
  InitWindow(1920, 1080, "Rdy");
  RenderTexture2D target = LoadRenderTexture(1920, 1080);

  SetTargetFPS(60);

  console_init();

  const char* include_paths[] = {
      fullpath(".\\raylib\\src"),
      fullpath(".\\project"),
      NULL,
  };
  const char* files[] = {
      fullpath(".\\project\\entry.c"),
      fullpath(".\\project\\player.c"),
      NULL,
  };

  allocate_pending_updates(files);

  char* nvim_config_fullpath = fullpath("ide");
  nvim_connection_setup(files, nvim_config_fullpath, &connection_handle);
  free(nvim_config_fullpath);

  DyibiccEnviromentData cc_env_data = {.include_paths = include_paths,
                                       .files = files,
                                       .dyibicc_include_dir = "libdyibicc\\include",
                                       .get_function_address = provide_function,
                                       .output_function = output_function,
                                       .use_ansi_codes = true};
  cc_ctx = dyibicc_set_environment(&cc_env_data);

  for (char** p = include_paths; *p; ++p)
    free(*p);
  for (char** p = files; *p; ++p)
    free(*p);

  bool first = true;

  last_compile_successful = dyibicc_update(cc_ctx, NULL, NULL);

  SetExitKey(0);
  while (!WindowShouldClose()) {
    if (!nvim_connection_poll(file_update_notification)) {
      break;
    }

    commit_last_change_for_each_file();

    // We render in to a texture so that non-mode-changing fullscreen
    // isn't a hassle in game render code.
    BeginTextureMode(target);

#define ENTRY_POINT_NAME "RdyEntryPoint"
    void* entry = NULL;
    if (last_compile_successful)
      entry = dyibicc_find_export(cc_ctx, ENTRY_POINT_NAME);

    if (last_compile_successful && entry) {
      console_error_set_visible(false);
      void (*p)(int) = (void (*)(int))entry;
      p(first ? 0 : 1);
      first = false;
    } else {
      console_error_set_visible(true);
      ClearBackground(DARKGRAY);
      if (last_compile_successful) {
        console_error_clear();
        console_error_printf("\033[1;31mentry point not found: \033[0m%s\n", ENTRY_POINT_NAME);
      }
    }

    console_update();

    EndTextureMode();

    BeginDrawing();
    Rectangle source_rect = {0.0f, 0.0f, (float)target.texture.width,
                             -(float)target.texture.height};
    Rectangle dest_rect = {0, 0, (float)GetRenderWidth(), (float)GetRenderHeight()};
    DrawTexturePro(target.texture, source_rect, dest_rect, (Vector2){0,0}, 0.0f, WHITE);
    EndDrawing();
  }

  dyibicc_free(cc_ctx);
  cc_ctx = NULL;

  nvim_connection_send_quit_and_shutdown(connection_handle);
  free_pending_updates();

  CloseWindow();

  return 0;
}
