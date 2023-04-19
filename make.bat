@echo off

set CFLAGS=/nologo /c /Ox /GL /Zi /MT /D_CRT_SECURE_NO_DEPRECATE /DPLATFORM_DESKTOP /DMPACK_EXTENSIONS=1
:: /fsanitize=address
:: /DMPACK_DEBUG=1

cl %CFLAGS% /W4 /WX ^
  /Iraylib\src ^
  /I..\dyibicc ^
  /Impack ^
  /Ilibdyibicc ^
  src\rdy_shell.c ^
  src\rdy_nvim_listener.c ^
  src\console.c ^
  mpack\mpack.c ^
  libdyibicc\libdyibicc.c ^
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
  mpack.obj ^
  raudio.obj ^
  rcore.obj ^
  rglfw.obj ^
  rmodels.obj ^
  rshapes.obj ^
  rtext.obj ^
  rtextures.obj ^
  utils.obj ^
  libdyibicc.obj ^
  winmm.lib user32.lib gdi32.lib shell32.lib onecore.lib /LTCG /DEBUG /out:rdy.exe ^
  || exit /b 1

del *.obj 2>nul
del vc140.pdb 2>nul
del *.ilk 2>nul
del *.exp 2>nul
del *.lib 2>nul
