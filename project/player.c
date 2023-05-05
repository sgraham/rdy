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
} Animation;

#define PW 32
#define PH 32
#define GRID 18
#define SCREEN_WIDTH 1872
#define SCREEN_HEIGHT 1080
#define ZOOM_WIDTH (SCREEN_WIDTH/4)
#define ZOOM_HEIGHT (SCREEN_HEIGHT/4)

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

// TODO: reusing a run frame
Rectangle anim_jump_rects[] = {
    {144, 386, PW, PH},
};
Animation anim_jump = {1, &anim_jump_rects};

Rectangle egg_rect = {360, 288, 9, 11};

double clamp_walk = 1.3;  // maximum walking speed
double clamp_fall_speed = 5;

// when holding button how fast to get to max speed
const double walk_acceleration = .3;
const double walk_deceleration = .15;  // when no button is held how fast to get to 0

const double jump_height = 5.8;  // 4 bricks
bool jump_early_end = false;

double current_h_speed = 0;  // how fast the player would like to move
double current_v_speed = 0;

const double fall_speed = .25;
const double fall_speed_min = .13;
const double fall_speed_max = .25;

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
double speed_for_apex = 2;  // if they are going slower than this and in the air
                            // we call this the apex

double current_h_speed;
double current_v_speed;

double x = 30;
double y = 50;

float screen_target_x;
bool changing_screens;

#define MAX_PARTICLES 300
typedef struct Particle {
  Rectangle* src_tex_rect;
  Vector2 position;
  Vector2 velocity;
  Vector2 gravity;
  int frames_until_death;
} Particle;
Particle particles[MAX_PARTICLES];

typedef struct Interp {
  float* into;
  float start;
  float end;
  int cur_tick;
  int total_ticks;
  double (*func)(double);
  void (*on_complete)(float*);
} Interp;
#define MAX_INTERPS 16
static Interp interps[MAX_INTERPS];

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
    [Helper_speedy_apex] = true,           [Helper_coyote_jump] = true,
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
#define COLL_R (7)
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

static Particle* find_particle_slot(void) {
  for (size_t i = 0; i < MAX_PARTICLES; ++i) {
    if (!particles[i].src_tex_rect) {
      return &particles[i];
    }
  }
  return NULL;
}

static void update_particles(void) {
  for (size_t i = 0; i < MAX_PARTICLES; ++i) {
    Particle*p = &particles[i];
    if (p->src_tex_rect) {
      if (p->frames_until_death-- == 0) {
        p->src_tex_rect = NULL;
        break;
      }
      p->position.x += p->velocity.x;
      p->position.y += p->velocity.y;
      p->velocity.x += p->gravity.x;
      p->velocity.y += p->gravity.y;
    }
  }
}

static void draw_particles(void) {
  for (size_t i = 0; i < MAX_PARTICLES; ++i) {
    Particle*p = &particles[i];
    if (p->src_tex_rect) {
      DrawTextureRec(texall, *p->src_tex_rect, p->position, WHITE);
    }
  }
}

static Interp* find_interp_slot(void) {
  for (int i = 0; i < MAX_INTERPS; ++i) {
    Interp* interp = &interps[i];
    if (!interp->into) {
      return interp;
    }
  }
  return NULL;
}

static void new_interpf(float* start_and_into,
                       float target,
                       int ticks,
                       double (*func)(double),
                       void (*on_complete)(float*)) {
  Interp* interp = find_interp_slot();
  if (!interp)
    return;
  interp->into = start_and_into;
  interp->start = *start_and_into;
  interp->end = target;
  interp->cur_tick = 0;
  interp->total_ticks = ticks;
  interp->func = func;
  interp->on_complete = on_complete;
}

static void set_changing_screen_off(float* p) {
  changing_screens = false;
}

double SineEaseInOut(double);
double QuadraticEaseOut(double);
double BounceEaseOut(double);

static void update_camera(Camera2D* cam) {
#define AT_EDGE (2*GRID)
#define PAST_EDGE (6*GRID)
  if (x + cam->offset.x/cam->zoom + COLL_R > ZOOM_WIDTH - AT_EDGE) {
    // Entering last grid on RHS
    changing_screens = true;
    new_interpf(&cam->offset.x,
               cam->offset.x - (ZOOM_WIDTH - PAST_EDGE) * cam->zoom, 40,
               SineEaseInOut, set_changing_screen_off);
  }
  if (x + cam->offset.x/cam->zoom + COLL_L < AT_EDGE) {
    float target = fminf(0, cam->offset.x + (ZOOM_WIDTH - PAST_EDGE) * cam->zoom);
    if (!FloatEquals(target, cam->offset.x)) {
      changing_screens = true;
      new_interpf(&cam->offset.x, target, 40, SineEaseInOut,
                  set_changing_screen_off);
    }
  }
  if (y + cam->offset.y/cam->zoom + COLL_B > ZOOM_HEIGHT - AT_EDGE) {
    changing_screens = true;
    new_interpf(&cam->offset.y,
                cam->offset.y - (ZOOM_HEIGHT - PAST_EDGE) * cam->zoom, 30,
                SineEaseInOut, set_changing_screen_off);
  }
  if (y + cam->offset.y/cam->zoom + COLL_T < AT_EDGE) {
    float target = fminf(0, cam->offset.y + (ZOOM_HEIGHT - PAST_EDGE) * cam->zoom);
    if (!FloatEquals(target, cam->offset.y)) {
      changing_screens = true;
      new_interpf(&cam->offset.y, target, 30, SineEaseInOut,
                  set_changing_screen_off);
    }
  }
}

static void update_interps(void) {
  for (int i = 0; i < MAX_INTERPS; ++i) {
    Interp* interp = &interps[i];
    if (!interp->into)
      continue;
    *interp->into =
        Lerp(interp->start, interp->end,
             interp->func((float)++interp->cur_tick / interp->total_ticks));
    QQ(*interp->into);
    if (interp->cur_tick == interp->total_ticks) {
      interp->on_complete(interp->into);
      interp->into = NULL;
    }
  }
}

void do_player_movement(Camera2D* cam) {
  update_interps();

  if (changing_screens) {
    goto draw_only;
  }

  bool want_left = IsGamepadButtonDown(0, GAMEPAD_BUTTON_LEFT_FACE_LEFT) ||
                   GetGamepadAxisMovement(0, GAMEPAD_AXIS_LEFT_X) < -0.5f;
  bool want_right = IsGamepadButtonDown(0, GAMEPAD_BUTTON_LEFT_FACE_RIGHT) ||
                    GetGamepadAxisMovement(0, GAMEPAD_AXIS_LEFT_X) > 0.5f;
  bool want_up = IsGamepadButtonDown(0, GAMEPAD_BUTTON_LEFT_FACE_UP) ||
                 GetGamepadAxisMovement(0, GAMEPAD_AXIS_LEFT_Y) < -0.5f;

  bool want_jump = IsGamepadButtonPressed(0, GAMEPAD_BUTTON_RIGHT_FACE_DOWN);
  bool jump_released =
      IsGamepadButtonReleased(0, GAMEPAD_BUTTON_RIGHT_FACE_DOWN);

  double egg_x = GetGamepadAxisMovement(0, GAMEPAD_AXIS_RIGHT_X);
  double egg_y = GetGamepadAxisMovement(0, GAMEPAD_AXIS_RIGHT_Y);
  bool want_egg_spray;
  double egg_angle = 0.0;
  want_egg_spray = sqrt(egg_x*egg_x + egg_y*egg_y) > 0.5;
  if (want_egg_spray) {
    egg_angle = atan2(egg_y, egg_x);
  }

  bool want_egg = IsGamepadButtonPressed(0, GAMEPAD_BUTTON_RIGHT_FACE_LEFT);

  if (IsKeyPressed(KEY_ESCAPE)) {
    x = 30;
    y = 50;
    current_h_speed = 0;
    current_v_speed = 0;
    cam->offset.x = 0;
    cam->offset.y = 0;
  }

  if (IsKeyPressed(KEY_F5)) {
    bounding_boxes = !bounding_boxes;
  }

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
  if (helper_on[Helper_coyote_jump] && grounded_frames_ago < 4 &&
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
  bool coyote_used = false;
  if (use_jump && can_jump) {
    current_v_speed = -(jump_height);
    touched_ground_since_last_jump = false;
    jump_early_end = false;

    if (grounded == false) {
      // coyote_jump was used
      coyote_used = true;
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
  if (coyote_used) {
    record_line_colour[record_frame] = GREEN;
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

  if (want_egg) {
    Particle* p = find_particle_slot();
    if (p) {
      p->src_tex_rect = &egg_rect;
      if (h_flip_render) {
        p->position.x = x - 8;
      } else {
        p->position.x = x - 2;
      }

      if (want_up) {
        p->position.y = y-30;

        p->velocity.x = 0;
        p->velocity.y = -8;
      } else {
        p->position.y = y-20;
        if (h_flip_render) {
          p->velocity.x = -6;
        } else {
          p->velocity.x = 6;
        }
        p->velocity.y = -3.5;
      }
      p->gravity.x = 0;
      p->gravity.y = .3;
      p->frames_until_death = 180;
    }
  }

  static int frames_since_last_egg_spray = 0;
  if (want_egg_spray) {
    if (frames_since_last_egg_spray > 0) {
      frames_since_last_egg_spray--;
    } else {
      frames_since_last_egg_spray = 12;
      Particle *p = find_particle_slot();
      p->src_tex_rect = &egg_rect;
      p->position.x = x - 5 + 8*cos(egg_angle);
      p->position.y = y - 22 + 3*sin(egg_angle);
      p->velocity.x = 7 * cos(egg_angle);
      p->velocity.y = 7 * sin(egg_angle);
      p->gravity.x = 0;
      p->gravity.y = .3;
      p->frames_until_death = 180;
    }
  }

  update_particles();

  while (frame_counter >= cur_anim->num_frames)
    frame_counter -= cur_anim->num_frames;

draw_only:
  if (h_flip_render) {
    Rectangle copy = cur_anim->rects[(int)frame_counter];
    copy.width = -copy.width;
    DrawTextureRec(texall, copy, (Vector2){(int)x - 16, (int)y - 32}, WHITE);
  } else {
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

    for (int i = 0; i < RECORD_COUNT; ++i) {
      int j = (i + 1) % RECORD_COUNT;
      if (i == record_frame) continue;
      DrawLine(record_line_x[i], record_line_y[i], record_line_x[j],
               record_line_y[j], record_line_colour[i]);
    }
  }

  draw_particles();

  update_camera(cam);
}
