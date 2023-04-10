#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "libdyibicc.h"
#include "raylib.h"

static DyibiccContext* cc_ctx;
static bool last_compile_successful;

extern bool nvim_connection_setup(const char* files[], const char* nvim_config_fullpath);
extern bool nvim_connection_poll(void (*file_update)(char* name, char* contents));

extern void console_init(void);
extern void console_update(void);
extern void console_set_override_visible(bool visible);
extern void console_clear(void);
extern int console_vprintf(int level, const char* fmt, va_list ap);
extern int console_errf(const char* fmt, ...);
extern int console_logf(const char* fmt, ...);
extern void console_shutdown(void);

static int output_function(int level, const char* fmt, va_list ap) {
  return console_vprintf(level, fmt, ap);
  //return vfprintf(stdout, fmt, ap);
}

static void Log(const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  console_vprintf(1, fmt, ap);
}

static void* provide_function(const char* name) {
#define X(n) if (strcmp(#n, name) == 0) return (void*)n;
  X(Log);

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

static void file_update_notification(char* filename, char* contents) {
  if (!last_compile_successful) {
    console_clear();
  }
  last_compile_successful = dyibicc_update(cc_ctx, filename, contents);
}

int main(void) {
  InitWindow(1920, 1080, "Rdy");
  SetTargetFPS(60);

  console_init();

  const char* include_paths[] = {
      fullpath(".\\raylib\\src"),
      fullpath("."),
      NULL,
  };
  const char* files[] = {
      fullpath(".\\entry.c"),
      NULL,
  };

  char* nvim_config_fullpath = fullpath("ide");
  nvim_connection_setup(files, nvim_config_fullpath);
  free(nvim_config_fullpath);

  DyibiccEnviromentData cc_env_data = {.include_paths = include_paths,
                                       .files = files,
                                       .entry_point_name = "RdyEntryPoint",
                                       .get_function_address = provide_function,
                                       .output_function = output_function};
  cc_ctx = dyibicc_set_environment(&cc_env_data);

  for (char**p = include_paths; *p; ++p)
    free(*p);
  for (char**p = files; *p; ++p)
    free(*p);

  bool first = true;

  last_compile_successful = dyibicc_update(cc_ctx, NULL, NULL);

  while (!WindowShouldClose()) {
    if (!nvim_connection_poll(file_update_notification)) {
      break;
    }

    BeginDrawing();

    if (last_compile_successful && cc_ctx->entry_point) {
      void (*p)(int) = (void (*)(int))cc_ctx->entry_point;
      p(first ? 0 : 1);
      first = false;
    } else {
      ClearBackground(VIOLET);
    }

    console_set_override_visible(!last_compile_successful);
    console_update();

    EndDrawing();
  }

  dyibicc_free(cc_ctx);
  cc_ctx = NULL;

  CloseWindow();

  return 0;
}
