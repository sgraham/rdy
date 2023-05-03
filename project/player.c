#ifdef __dyibicc__
#include <reflect.h>

#define QQ(expr)                                \
  qq_eval(                                      \
      __FILE__, __LINE__, _ReflectTypeOf(expr), \
      expr)

void qq_eval(const char* file, int line, int num_args, ...);
#else
#define QQ(expr) ((void)expr)
#endif

#include "raylib.h"
#include "raymath.h"

typedef struct Animation {
  int num_frames;
  Rectangle* rects;
} Animation;

#define PW 32
#define PH 32
#define GRID 18

Rectangle anim_idle_rects[] = {{176, 162, PW, PH}, {176, 194, PW, PH},
                          {176, 226, PW, PH}, {176, 258, PW, PH},
                          {176, 290, PW, PH}, {176, 322, PW, PH}};
Animation anim_idle = {6, &anim_idle_rects};

Rectangle anim_run_rects[] = {
    {144, 162, PW, PH}, {144, 194, PW, PH}, {144, 226, PW, PH},
    {144, 258, PW, PH}, {144, 290, PW, PH}, {144, 322, PW, PH},
    {144, 354, PW, PH}, {144, 386, PW, PH}, {144, 418, PW, PH},
    {144, 450, PW, PH},
};
Animation anim_run = {10, &anim_run_rects};

extern Texture2D texall;
extern bool level_is_set(int x, int y, int layer);

double player_x = 0;
double player_y = 0;

// when holding button how fast to get to max speed
double walk_acceleration = 2;

// when no button is held how fast to get to 0
double walk_deceleration = 1.4;

// double fall_speed_min = 0.8;
double fall_speed_max = 1.2;

// maximum walking speed
double clamp_walk = 2;

double clamp_fall_speed = 20;

double jump_height = 19;

double current_h_speed = 0;
double current_v_speed = 0;
double fall_speed = 1.2;//fall_speed_max;

bool touched_ground_since_last_jump = true;

bool jump_early_end;

bool grounded;

double DT = 1;

bool render_h_flip = true;
bool level_edit_mode = false;

void do_player_movement(void) {
  bool want_left = IsGamepadButtonDown(0, GAMEPAD_BUTTON_LEFT_FACE_LEFT) ||
                   GetGamepadAxisMovement(0, GAMEPAD_AXIS_LEFT_X) < -0.5f;
  bool want_right = IsGamepadButtonDown(0, GAMEPAD_BUTTON_LEFT_FACE_RIGHT) ||
                   GetGamepadAxisMovement(0, GAMEPAD_AXIS_LEFT_X) > 0.5f;

  bool want_jump = IsGamepadButtonDown(0, GAMEPAD_BUTTON_RIGHT_FACE_DOWN);
  bool jump_released = !IsGamepadButtonDown(0, GAMEPAD_BUTTON_RIGHT_FACE_DOWN);

  if (want_left || want_right) {
    current_h_speed += (want_right - want_left) * (walk_acceleration * DT);
    current_h_speed = Clamp(current_h_speed, -(clamp_walk * DT), (clamp_walk * DT));
  } else {
    // slow them down
    if (current_h_speed > walk_deceleration) {
      current_h_speed -= (walk_deceleration * DT);
    } else if (current_h_speed < -walk_deceleration) {
      current_h_speed += (walk_deceleration * DT);
    } else {
      current_h_speed = 0;
    }
  }

  clamp_walk = 2;
  jump_height = 4.3;
  fall_speed_max = .15;
//player_y = 16;

  if (current_h_speed > 0) {
    render_h_flip = true;
  } else if (current_h_speed < 0) {
    render_h_flip = false;
  }

  fall_speed = fall_speed_max;
  current_v_speed += fall_speed / DT;

  if (current_v_speed > clamp_fall_speed) {
    current_v_speed = clamp_fall_speed;
  }

  bool can_jump = false;
  int grid_player_below_y = (player_y) / GRID;
  int grid_player_cur_x = player_x / GRID;
  if (level_is_set(grid_player_cur_x, grid_player_below_y, 0)) {
    can_jump = true;
  }

  if (!touched_ground_since_last_jump) {
    can_jump = false;
  }

  if (want_jump && can_jump) {
    current_v_speed = -jump_height;
    jump_early_end = true;
    touched_ground_since_last_jump = false;
  }

  if (jump_released && !grounded && current_v_speed < 0) {
    jump_early_end = true;
    current_v_speed = 0;
  }

#if 0
  if (want_roar) {
    SDL_GameControllerRumble(gamecontroller, 0xFFFF, 0xFFFF, 500);
  }
#endif

  player_x += current_h_speed;
  player_y += current_v_speed;

  QQ(player_x);
  QQ(player_y);
  QQ(current_v_speed);
  QQ((int)can_jump);
  QQ((int)want_jump);
  QQ((int)jump_released);
  QQ((int)touched_ground_since_last_jump);
  QQ((int)grounded);

  int grid_player_below_y = (player_y) / GRID;
  int grid_player_cur_x = player_x / GRID;
  QQ(grid_player_below_y);
  QQ(grid_player_cur_x);
  QQ((int)level_is_set(grid_player_cur_x, grid_player_below_y, 0));
  if (level_is_set(grid_player_cur_x, grid_player_below_y, 0)) {
    player_y = grid_player_below_y * GRID;
    QQ(player_y);
    current_v_speed = 0.0;
    grounded = true;
    if (jump_released) {
      touched_ground_since_last_jump = true;
    }
  } else {
    grounded = false;
  }

#if 0
  if (player_x > SCREEN_WIDTH - 48 * 2) {
    player_x = SCREEN_WIDTH - 48 * 2;
    current_h_speed = 0;
  } else if (player_x < 0) {
    player_x = 0;
    current_h_speed = 0;
  }
#endif

#if 0
  static double counter;
  counter += 10.0 / 60.0;
  if (counter >= anim_idle.num_frames)
    counter -= anim_idle.num_frames;
  DrawTextureRec(texall, anim_idle.rects[(int)counter], (Vector2){220, 0},
                 WHITE);
#endif

  static double counter2;
  counter2 += 10.0 / 60.0;  // QQ(counter2);
  if (counter2 >= anim_run.num_frames)
    counter2 -= anim_run.num_frames;
  DrawTextureRec(texall, anim_run.rects[(int)counter2],
                 (Vector2){-16+player_x, -32+player_y}, WHITE);
}
