@echo off
pushd ..\dyibicc
call make.bat
popd

set CFLAGS=/nologo /fsanitize=address /c /Ox /GL /Zi /MT /D_CRT_SECURE_NO_DEPRECATE /DPLATFORM_DESKTOP /DMPACK_EXTENSIONS=1
:: /DMPACK_DEBUG=1

cl %CFLAGS% /W4 /WX ^
  /Iraylib\src ^
  /I..\dyibicc ^
  /Impack\src\mpack ^
  rdy_shell.c ^
  rdy_nvim_listener.c ^
  console.c ^
  || exit /b 1

cl %CFLAGS% /W4 /WX ^
    mpack\src\mpack\mpack-common.c ^
    mpack\src\mpack\mpack-expect.c ^
    mpack\src\mpack\mpack-node.c ^
    mpack\src\mpack\mpack-platform.c ^
    mpack\src\mpack\mpack-reader.c ^
    mpack\src\mpack\mpack-writer.c^
    || exit /b 1

cl %CFLAGS% /W1 ^
  /Iraylib\src ^
  /Iraylib\src\external\glfw\include ^
  /W1 ^
  raylib\src\raudio.c ^
  raylib\src\rcore.c ^
  raylib\src\rglfw.c ^
  raylib\src\rmodels.c ^
  raylib\src\rshapes.c ^
  raylib\src\rtext.c ^
  raylib\src\rtextures.c ^
  raylib\src\utils.c ^
  || exit /b 1

link /nologo ^
  rdy_shell.obj ^
  rdy_nvim_listener.obj ^
  console.obj ^
  mpack-common.obj ^
  mpack-expect.obj ^
  mpack-node.obj ^
  mpack-platform.obj ^
  mpack-reader.obj ^
  mpack-writer.obj ^
  raudio.obj ^
  rcore.obj ^
  rglfw.obj ^
  rmodels.obj ^
  rshapes.obj ^
  rtext.obj ^
  rtextures.obj ^
  utils.obj ^
  ..\dyibicc\alloc.obj ^
  ..\dyibicc\codegen.win32.obj ^
  ..\dyibicc\dyo.obj ^
  ..\dyibicc\hashmap.obj ^
  ..\dyibicc\link.obj ^
  ..\dyibicc\main.obj ^
  ..\dyibicc\parse.obj ^
  ..\dyibicc\preprocess.obj ^
  ..\dyibicc\tokenize.obj ^
  ..\dyibicc\type.obj ^
  ..\dyibicc\unicode.obj ^
  ..\dyibicc\util.obj ^
  winmm.lib user32.lib gdi32.lib shell32.lib onecore.lib /LTCG /DEBUG /out:rdy.exe ^
  || exit /b 1

del *.obj 2>nul
del vc140.pdb 2>nul
del *.ilk 2>nul
