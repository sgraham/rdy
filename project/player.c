#ifdef __dyibicc__
#include <reflect.h>

#define QQ(expr) qq_eval(__FILE__, __LINE__, _ReflectTypeOf(expr), expr)

void qq_eval(const char* file, int line, int num_args, ...);
#else
#define QQ(expr) ((void)expr)
#endif

#include <math.h>
#include "raylib.h"
#include "raymath.h"

typedef struct Animation {
  int num_frames;
  Rectangle* rects;
  char* name;
} Animation;

#define PW 32
#define PH 32
#define GRID 18

Rectangle anim_idle_rects[] = {{176, 162, PW, PH}, {176, 194, PW, PH},
                               {176, 226, PW, PH}, {176, 258, PW, PH},
                               {176, 290, PW, PH}, {176, 322, PW, PH}};
Animation anim_idle = {6, &anim_idle_rects, "idle"};

Rectangle anim_run_rects[] = {
    {144, 162, PW, PH}, {144, 194, PW, PH}, {144, 226, PW, PH},
    {144, 258, PW, PH}, {144, 290, PW, PH}, {144, 322, PW, PH},
    {144, 354, PW, PH}, {144, 386, PW, PH}, {144, 418, PW, PH},
    {144, 450, PW, PH},
};
Animation anim_run = {10, &anim_run_rects, "run"};

// TODO: reusing a run frame
Rectangle anim_jump_rects[] = {
    {144, 386, PW, PH},
};
Animation anim_jump = {1, &anim_jump_rects, "jump"};

double clamp_walk = 2;  // maximum walking speed
double clamp_fall_speed = 6;

double walk_acceleration =
    2;  // when holding button how fast to get to max speed
double walk_deceleration = 1.4;  // when no button is held how fast to get to 0

double jump_height = 5.1;  // 4 bricks
bool jump_early_end = false;

double current_h_speed = 0;  // how fast the player would like to move
double current_v_speed = 0;

double fall_speed = .18;
double fall_speed_min = .18;
double fall_speed_max = .25;

bool grounded = false;
bool touched_ground_since_last_jump = false;

// recording how many frames ago these things happened
int grounded_frames_ago = 999;
int landed_frames_ago = 999;
int apex_frames_ago =
    999;  // 0 if on floor and on way up (only counts on way down)
int jump_frames_ago = 999;
int frames_going_down = 999;

bool at_apex = false;       // this isnt the apex but the top 4th
double speed_for_apex = 6;  // if they are going slower than this and in the air
                            // we call this the apex

double current_h_speed;
double current_v_speed;
bool at_apex;
bool grounded;

double x = 30;
double y = 50;

int record_frame;
#define RECORD_COUNT 150
double record_line_x[RECORD_COUNT];
double record_line_y[RECORD_COUNT];
Color record_line_colour[RECORD_COUNT];

typedef enum Helper {
  Helper_anti_gravity_apex,
  Helper_early_fall,
  Helper_jump_buffering,
  Helper_sticky_feet,
  Helper_speedy_apex,
  Helper_coyote_jump,
  Helper_clamp_fall_speed,
  Helper_catch_missed_jump,
  Helper_bumped_head_on_corner,

  Helper_NumElements
} Helper;

bool helper_on[Helper_NumElements] = {
    [Helper_anti_gravity_apex] = true,      [Helper_early_fall] = true,
    [Helper_jump_buffering] = false,        [Helper_sticky_feet] = false,
    [Helper_speedy_apex] = false,           [Helper_coyote_jump] = true,
    [Helper_clamp_fall_speed] = true,       [Helper_catch_missed_jump] = false,
    [Helper_bumped_head_on_corner] = false,
};

double DT = 1.0;

bool bounding_boxes;

Animation* cur_anim = &anim_idle;

extern Texture2D texall;
extern bool level_is_set(int x, int y, int layer);
extern bool level_is_set(int x, int y, int layer);

#define COLL_L (-7)
#define COLL_T (-32)
#define COLL_R (11)
#define COLL_B (0)

bool place_free(double x, double y) {
  int grid_x = x / GRID;
  int grid_y = y / GRID;
  return !level_is_set(grid_x, grid_y, 0);
}

#if 0
#define MIN(x, y) ((x) < (y) ? (x) : (y))

double ray_intersect_level(double origin_x,
                           double origin_y,
                           double dist_x,
                           double dist_y) {
  int grid_origin_x = origin_x / GRID;
  int grid_origin_y = origin_y / GRID;
  double end_x = origin_x + dist_x;
  double end_y = origin_y + dist_y;
  QQ(origin_x);
  QQ(origin_y);
  QQ(end_x);
  QQ(end_y);
  int grid_end_x = end_x / GRID;
  int grid_end_y = end_y / GRID;
  if (grid_origin_x == grid_end_x && grid_origin_y == grid_end_y) {
    if (!level_is_set(grid_origin_x, grid_origin_y, 0))
       return 1.0;
  }
  if (grid_origin_x == grid_end_x && grid_origin_y != grid_end_y) {
    // vertical
    double wall_y = grid_end_y * GRID;
    if (dist_y < 0) wall_y -= GRID;
    return wall_y / (end_y - origin_y);
  } else if (grid_origin_y == grid_end_y && grid_origin_x != grid_end_x) {
    // horizontal
    double wall_x = grid_end_x * GRID;
    if (dist_x < 0) wall_x -= GRID;
    return wall_x / (end_x - origin_x);
  } else {
    //Log("%s", "unhandled intersect (%f %f)+(%f %f)", origin_x, origin_y, dist_x, dist_y);
    return 0.0;
  }
}

void move_contact_solid(double direction, double amount) {
  QQ(amount);
  if (direction == 270) {
    // Check bottom left and bottom right of BB.
    double fraction_left = ray_intersect_level(x + COLL_L, y + COLL_T, 0, amount);
    double fraction_right =
        ray_intersect_level(x + COLL_L, y + COLL_B, 0, amount);
    y += amount * MIN(fraction_left, fraction_right);
  } else if (direction == 0) {
    // Check top and bottom right of BB.
    double fraction_top =
        ray_intersect_level(x + COLL_R, y + COLL_T, amount, 0);
    double fraction_bottom =
        ray_intersect_level(x + COLL_R, y + COLL_B, amount, 0);
    x += amount * MIN(fraction_top, fraction_bottom);
  } else if (direction == 180) {
    // Check top and bottom left of BB.
    double fraction_top =
        ray_intersect_level(x + COLL_L, y + COLL_T, -amount, 0);
    double fraction_bottom =
        ray_intersect_level(x + COLL_L, y - 1 + COLL_B, -amount, 0);
    x += -amount * MIN(fraction_top, fraction_bottom);
  } else if (direction == 180) {
    // Check top left and right of BB.
    double fraction_left =
        ray_intersect_level(x + COLL_L, y + COLL_T, 0, amount);
    double fraction_right =
        ray_intersect_level(x + COLL_R, y + COLL_T, 0, amount);
    y += -amount * MIN(fraction_left, fraction_right);
  }
#if 0
  QQ(direction);
  QQ(amount);
  double dx = 0;
  double dy = 0;
  if (direction == 0) {
    dx = .01;
  } else if (direction == 180) {
    dx = -.01;
  } else if (direction == 90) {
    dy = -.01;
  } else if (direction == 270) {
    dy = .01;
  }

  QQ(dx);
  QQ(dy);

  while (amount > 0) {
    if (!place_free(x+dx, y+dy)) return;
    x += dx;
    y += dy;
    amount -= dx;
    amount -= dy;
  }
#endif
}

#endif
void do_player_movement(void) {
  bool want_left = IsGamepadButtonDown(0, GAMEPAD_BUTTON_LEFT_FACE_LEFT) ||
                   GetGamepadAxisMovement(0, GAMEPAD_AXIS_LEFT_X) < -0.5f;
  bool want_right = IsGamepadButtonDown(0, GAMEPAD_BUTTON_LEFT_FACE_RIGHT) ||
                    GetGamepadAxisMovement(0, GAMEPAD_AXIS_LEFT_X) > 0.5f;

  bool want_jump = IsGamepadButtonPressed(0, GAMEPAD_BUTTON_RIGHT_FACE_DOWN);
  bool jump_released =
      IsGamepadButtonReleased(0, GAMEPAD_BUTTON_RIGHT_FACE_DOWN);

  if (IsKeyPressed(KEY_ESCAPE)) {
    x = 30;
    y = 50;
    current_h_speed = 0;
    current_v_speed = 0;
  }

  if (IsKeyPressed(KEY_F5)) {
    bounding_boxes = !bounding_boxes;
  }

  walk_acceleration = .3;
  walk_deceleration = .3;
  clamp_walk = 1.3;
  clamp_fall_speed = 6;

  if (want_left || want_right) {  // they are moving left / right
    current_h_speed += (want_right - want_left) * (walk_acceleration * DT);
    current_h_speed =
        Clamp(current_h_speed, -(clamp_walk * DT), (clamp_walk * DT));
  } else {  // not moving
    // slow them down
    if (current_h_speed > walk_deceleration) {
      current_h_speed -= (walk_deceleration * DT);
    } else if (current_h_speed < -walk_deceleration) {
      current_h_speed += (walk_deceleration * DT);
    } else {
      current_h_speed = 0;
    }
  }

  // Am I at the apex
  at_apex = false;
  fall_speed = fall_speed_max;
  if (grounded == false) {
    if (fabs(current_v_speed) < speed_for_apex &&
        helper_on[Helper_anti_gravity_apex]) {
      at_apex = true;
      fall_speed = fall_speed_min;
    }
  }

  // gravity
  current_v_speed += (fall_speed / DT);

  // max fall speed
  if (current_v_speed > clamp_fall_speed &&
      helper_on[Helper_clamp_fall_speed]) {
    current_v_speed = clamp_fall_speed;
  }

  // work out if I am allowed to jump
  bool can_jump = false;
  if (grounded == true) {
    can_jump = true;
  }
  if (helper_on[Helper_coyote_jump] && grounded_frames_ago < 8 &&
      grounded == false && helper_on[Helper_coyote_jump]) {
    can_jump = true;
  }
  if (touched_ground_since_last_jump == false) {
    can_jump = false;
  }

  bool use_jump = want_jump;  // for buffering w
  if (jump_frames_ago < 7 && frames_going_down > 2 &&
      helper_on[Helper_jump_buffering]) {  // can only jump buffer after going
                                           // down for a little
    use_jump = true;
  }

  // DO THE JUMP
  if (use_jump && can_jump) {
    current_v_speed = -(jump_height);
    touched_ground_since_last_jump = false;
    jump_early_end = false;

    if (grounded == false) {
      // coyote_jump was used
    }
  }

  // Better movment at apex
  if (at_apex && helper_on[Helper_speedy_apex]) {
    if (current_h_speed > 1 && want_left) {
      current_h_speed -= 5;
    }
    if (current_h_speed < -1 && want_right) {
      current_h_speed += 5;
    }
  }

  // early end jump
  if (jump_released && grounded == false && jump_early_end == false &&
      current_v_speed < 0 && helper_on[Helper_early_fall]) {
    jump_early_end = true;

    current_v_speed = 0;
  }

  double move_to_x = x + (current_h_speed * DT);
  double move_to_y = y + (current_v_speed * DT);

  // hit floor sticky feet
  if (helper_on[Helper_sticky_feet] &&
      !place_free(move_to_x, move_to_y + 1)) {  // I'm going to hit the floor
    if (landed_frames_ago < 6) {  // just hit the ground this frame
      if (want_left &&
          current_h_speed > 0) {  // you are moving one direction but pressing
                                  // the button for other way
        move_to_x -=
            clamp_walk;  // ability to instantly change direction when landing
        current_h_speed = -clamp_walk;
      }

      if (want_right &&
          current_h_speed < 0) {  // you are moving one direction but pressing
                                  // the button for other way
        move_to_x +=
            clamp_walk;  // ability to instantly change direction when landing
        current_h_speed = clamp_walk;
      }
    }
  }

  // --- Actually move the person
  // from the control checks we know where the player should be

  double dx = move_to_x - x;
  double dy = move_to_y - y;

  if (dx != 0.0) {
    x += dx;

    int map_x = (dx > 0 ? (x + COLL_R) : (x + COLL_L)) / GRID;
    int map_y_top = (y + (COLL_B - COLL_T) / 2 + COLL_T) / GRID;
    int map_y_middle = (y + COLL_T) / GRID;
    int map_y_bottom = (y + COLL_B - 1) / GRID;

    bool hit = false;

    if (level_is_set(map_x, map_y_top, 0)) {
      hit = true;
    }

    if (level_is_set(map_x, map_y_middle, 0)) {
      hit = true;
    }

    if (level_is_set(map_x, map_y_bottom, 0)) {
      hit = true;
    }

    if (hit) {
      double adjust = (dx > 0) ? -COLL_R : -COLL_L + GRID;
      x = map_x * GRID + adjust;
    }
  }

  if (dy != 0.0) {
    y += dy;

    int map_x_left = (x + COLL_L + 1) / GRID;
    int map_x_middle = (x + (COLL_R - COLL_L) / 2 + COLL_L) / GRID;
    int map_x_right = (x + COLL_R - 1) / GRID;
    int map_y = (dy > 0 ? (y + COLL_B) : (y + COLL_T)) / GRID;

    bool hit = false;

    if (level_is_set(map_x_left, map_y, 0)) {
      hit = true;
    }

    if (level_is_set(map_x_middle, map_y, 0)) {
      hit = true;
    }

    if (level_is_set(map_x_right, map_y, 0)) {
      hit = true;
    }

    if (hit) {
      double adjust = (dy > 0) ? -COLL_B : -COLL_T + GRID;
      y = map_y * GRID + adjust;
    }
  }

#if 0
  if (place_free(move_to_x, move_to_y)) {
    // player didnt hit a wall so move them
    x = move_to_x;
    y = move_to_y;
  } else
  {
    // I've hit a wall so work out where I should be
    double xmoveamount = move_to_x - x;
    QQ(xmoveamount);
    if (xmoveamount > 0) {
      move_contact_solid(0, xmoveamount);
      // current_h_speed = 0
    } else if (xmoveamount < 0) {
      move_contact_solid(180, -xmoveamount);
      // current_h_speed = 0
    }
    // doing this twice so the player slides along the wall rather than stopping
    // if I collide in one direction and not the other
    double ymoveamount = move_to_y - y;
    QQ(ymoveamount);
    if (ymoveamount > 0) {
      move_contact_solid(270, ymoveamount);
    } else if (ymoveamount < 0) {
      move_contact_solid(90, -ymoveamount);
    }
  }
#endif

  // --- On ground
  if (place_free(x + COLL_L + 1, y + COLL_B + 1) &&
      place_free(x + COLL_R - 1, y + COLL_B + 1)) {
    grounded = false;
  } else {
    if (grounded == false) {
      landed_frames_ago = 0;
    }
    grounded = true;
    grounded_frames_ago = 0;
    current_v_speed = 0;
    touched_ground_since_last_jump = true;
  }

#if 0
  // --- Teleport me to other places
  if (current_v_speed < 0) {  // going up
    if (helper_on[Helper_bumped_head_on_corner] &&
        place_free(x, y - 1) == false) {  // they are about to hit a wall
      double bump_amount = 30;
      if (place_free(move_to_x + bump_amount, move_to_y - 1) == true) {
        x += bump_amount;
        y -= 1;
        move_contact_solid(180, bump_amount);
      }
      if (place_free(move_to_x - bump_amount, move_to_y - 1) == true) {
        x -= bump_amount;
        y -= 1;
        move_contact_solid(0, bump_amount);
      }
    }
  }
  if (current_v_speed > 0) {  // going down
    double bump_amount = 30;
    if (current_h_speed > 1) {              // going right
      if (place_free(x + 1, y) == false) {  // they are about to hit a wall
        if (helper_on[Helper_catch_missed_jump] &&
            place_free(move_to_x, move_to_y - bump_amount) == true) {
          x = move_to_x;
          y = move_to_y - (bump_amount + 1);
          move_contact_solid(270, bump_amount + 2);
        }
      }
    }
    if (current_h_speed < -1) {             // going left
      if (place_free(x - 1, y) == false) {  // they are about to hit a wall
        if (helper_on[Helper_catch_missed_jump] &&
            place_free(move_to_x, move_to_y - bump_amount) == true) {
          x = move_to_x;
          y = move_to_y - (bump_amount + 1);
          move_contact_solid(270, bump_amount + 2);
        }
      }
    }
  }
#endif

  // bumped head
  if (current_v_speed < 0 && (!place_free(x + COLL_R - 1, y + COLL_T - 1) ||
                              !place_free(x + COLL_L + 1, y + COLL_T - 1))) {
    current_v_speed = 0;
  } else if (current_v_speed > 0 &&
             (!place_free(x + COLL_R - 1, y + COLL_B + 1) ||
              !place_free(x + COLL_L + 1, y + COLL_B + 1))) {  // on ground
    current_v_speed = 0;
  }
  // touched wall
  if (current_h_speed > 0 && (!place_free(x + COLL_R + 1, y + COLL_B - 1) ||
                              !place_free(x + COLL_R + 1, y + COLL_T + 1))) {
    current_h_speed = 0;
  } else if (current_h_speed < 0 &&
             (!place_free(x + COLL_L - 1, y + COLL_B - 1) ||
              !place_free(x + COLL_L - 1, y + COLL_T + 1))) {
    current_h_speed = 0;
  }

  // for buffering stuff next frame
  grounded_frames_ago++;  // count how long I've been in air
  if (at_apex || (grounded && landed_frames_ago > 2) || current_v_speed < 0) {
    apex_frames_ago = 0;
  } else if (current_v_speed > 0) {  // going down
    apex_frames_ago++;
  }
  if ((grounded && landed_frames_ago > 2) || current_v_speed < 0) {
    frames_going_down = 0;
  } else if (current_v_speed > 0) {  // going down
    frames_going_down++;
  }
  if (grounded) {
    landed_frames_ago++;  // count how long I've been on ground
  }
  if (want_jump) {
    jump_frames_ago = 0;
  } else {
    jump_frames_ago++;
  }

  record_frame++;
  if (record_frame > RECORD_COUNT) {
    record_frame = 0;
  }
  record_line_x[record_frame] = x;
  record_line_y[record_frame] = y;
  record_line_colour[record_frame] = WHITE;
  if (at_apex) {
    record_line_colour[record_frame] = BLUE;
  }
  if (current_v_speed >= clamp_fall_speed - 0.5) {
    record_line_colour[record_frame] = RED;
  }

  static double frame_counter;
  static h_flip_render = false;
  frame_counter += 10.0 / 60.0;

  if (current_h_speed > 0)
    h_flip_render = false;
  else if (current_h_speed < 0)
    h_flip_render = true;
  if (grounded) {
    if (current_h_speed == 0) {
      cur_anim = &anim_idle;
    } else {
      cur_anim = &anim_run;
    }
  } else {
    cur_anim = &anim_jump;
  }
  while (frame_counter >= cur_anim->num_frames)
    frame_counter -= cur_anim->num_frames;
  if (h_flip_render) {
    Rectangle copy = cur_anim->rects[(int)frame_counter];
    copy.width = -copy.width;
    DrawTextureRec(texall, copy, (Vector2){(int)x - 16, (int)y - 32}, WHITE);
  } else {
    QQ(cur_anim->rects[(int)frame_counter].x);
    QQ(cur_anim->rects[(int)frame_counter].width);
    DrawTextureRec(texall, cur_anim->rects[(int)frame_counter],
                   (Vector2){(int)x - 16, (int)y - 32}, WHITE);
  }
  if (bounding_boxes) {
    Rectangle copy = cur_anim->rects[(int)frame_counter];
    copy.x = (int)x - 16;
    copy.y = (int)y - 32;
    // DrawRectangleRec(copy, Fade(RED, .4f));
    // DrawCircle((int)x+1, (int)y-10, 10, Fade(RED, .4f));
    // DrawCircle((int)x+1, (int)y-22, 10, Fade(RED, .4f));
    DrawRectangleLines((int)x + COLL_L, (int)y + COLL_T, COLL_R - COLL_L,
                       COLL_B - COLL_T, Fade(BLUE, .4f));
    DrawText(cur_anim->name, (int)x + COLL_L, (int)y + COLL_T - 10, 10,
             Fade(BLUE, .4f));
  }
}
