#include <stdlib.h>
#include <time.h>
#include <windows.h>

#include "mpack-node.h"
#include "mpack-writer.h"

static mpack_tree_t nvim_tree;
static uint32_t message_id;
static char file_receive_buffer[2 << 20];

// Buffer sizes for mpack reading.
#define NVIM_MAX_SIZE (64 * 1024 * 1024)
#define NVIM_MAX_NODES 1024 * 1024

static size_t do_nonblocking_read(mpack_tree_t* tree, char* buffer, size_t count) {
  HANDLE pipe = mpack_tree_context(tree);

  DWORD bytes_avail;
  BOOL ok = PeekNamedPipe(pipe, NULL, 0, NULL, &bytes_avail, NULL);
  if (!ok) {
    mpack_tree_flag_error(tree, mpack_error_io);
    return 0;
  }
  if (bytes_avail == 0) {
    return 0;
  }

  DWORD bytes_read;
  ok = ReadFile(pipe, buffer, (DWORD)count, &bytes_read, NULL);
  if (!ok && GetLastError() != ERROR_MORE_DATA) {
    mpack_tree_flag_error(tree, mpack_error_io);
  }
  return bytes_read;
}

static void send_message_and_free(HANDLE pipe, char* data, size_t size) {
  // printf("%zu byte message\n", size);
  DWORD bytes_written;
  bool ok = WriteFile(pipe, data, (DWORD)size, &bytes_written, NULL);
  // printf("  %d bytes written\n", bytes_written);
  if (!ok) {
    fprintf(stderr, "WriteFile to pipe failed. GLE=%d\n", GetLastError());
    return;
  }
  free(data);
}

static bool send_subscribe(HANDLE pipe) {
  char* data;
  size_t size;
  mpack_writer_t writer;
  mpack_writer_init_growable(&writer, &data, &size);

  mpack_build_array(&writer);
  mpack_write_u32(&writer, 0);  // "Request"
  mpack_write_u32(&writer, ++message_id);
  mpack_write_cstr(&writer, "nvim_subscribe");
  mpack_build_array(&writer);
  // This is triggered by an autocmd and then sent here by rpcnotify.
  mpack_write_cstr(&writer, "EventFileUpdate");
  mpack_complete_array(&writer);
  mpack_complete_array(&writer);

  if (mpack_writer_destroy(&writer) != mpack_ok) {
    fprintf(stderr, "An error occurred encoding the data!\n");
    return false;
  }

  send_message_and_free(pipe, data, size);

  return true;
}

static bool launch_nvim(const char* pipename,
                        const char* files[],
                        const char* nvim_config_fullpath) {
  char nvim_exe_name[256];
  sprintf(nvim_exe_name, "%s\\nvim-win64\\bin\\nvim-qt.exe", nvim_config_fullpath);
  char command_line[2048];
  sprintf(command_line, "\"%s\" -- --listen %s ", nvim_exe_name, pipename);
  for (const char** p = files; *p; ++p) {
    strcat(command_line, *p);
    strcat(command_line, " ");
  }
  char envblock[1024] = {0};
  strcpy(envblock, "XDG_CONFIG_HOME=");
  strcat(envblock, nvim_config_fullpath);
  char* next = &envblock[strlen(envblock) + 1];
  strcpy(next, "XDG_DATA_HOME=");
  strcat(next, nvim_config_fullpath);
  strcat(next, "\\share\0");
  STARTUPINFO si = {0};
  si.cb = sizeof(STARTUPINFO);
  PROCESS_INFORMATION pi = {0};
  // TODO pi handles leaked
  return CreateProcess(nvim_exe_name, command_line, NULL, NULL, TRUE, 0, envblock, NULL, &si, &pi);
}

bool nvim_connection_setup(const char* files[],
                           const char* nvim_config_fullpath,
                           HANDLE* connection_handle) {
  // Write the "project" file, which init.lua uses to determine what notifications to send.
  FILE* f = fopen("__filelist.lua", "w");
  fprintf(f, "return {\n");
  for (const char** p = files; *p; ++p) {
    fprintf(f, "  [ [[%s]] ] = true%s\n", *p, *(p + 1) ? "," : "");
  }
  fprintf(f, "}\n");
  fclose(f);

  HANDLE pipe;
  srand((unsigned int)time(NULL));
  char pipename[256];
  sprintf(pipename, "\\\\.\\pipe\\nvim-rdy-%d", rand());

  if (!launch_nvim(pipename, files, nvim_config_fullpath)) {
    return false;
  }

  // Try to open a named pipe; wait for it, if necessary.
  int count = 0;
  for (;;) {
  again:
    pipe = CreateFile(pipename, GENERIC_READ | GENERIC_WRITE, 0, NULL, OPEN_EXISTING, 0, NULL);
    if (pipe != INVALID_HANDLE_VALUE)
      break;
    if (GetLastError() == ERROR_FILE_NOT_FOUND) {
      // Try until nvim starts.
      Sleep(10);
      ++count;
      if (count < 1000)
        goto again;
    }
    if (GetLastError() != ERROR_PIPE_BUSY) {
      fprintf(stderr, "Could not open pipe. GLE=%d\n", GetLastError());
      return false;
    }

    // All pipe instances are busy, so wait for 20 seconds.
    if (!WaitNamedPipe(pipename, 20000)) {
      fprintf(stderr, "Could not open pipe: 20 second wait timed out.");
      return false;
    }
  }

  if (!send_subscribe(pipe))
    return false;

  mpack_tree_init_stream(&nvim_tree, &do_nonblocking_read, pipe, NVIM_MAX_SIZE, NVIM_MAX_NODES);
  *connection_handle = pipe;
  return true;
}

static void got_message(mpack_node_t root, void (*callback)(char*, char*)) {
  // printf("-------------\n");
  // mpack_node_print_to_stdout(root);
  //  Ignore everything except our custom event generated from init.lua.
  if (mpack_node_array_length(root) == 3 && mpack_node_u32(mpack_node_array_at(root, 0)) == 2) {
    // I think "2" means event, but I can't find that documented anywhere.
    mpack_node_t evname = mpack_node_array_at(root, 1);
    if (strncmp(mpack_node_str(evname), "EventFileUpdate", mpack_node_strlen(evname)) == 0) {
      mpack_node_t event_data = mpack_node_array_at(root, 2);
      mpack_node_t filename_node = mpack_node_array_at(event_data, 1);
      char* name_copy = mpack_node_cstr_alloc(filename_node, 1024);
      mpack_node_t filecontents_node = mpack_node_array_at(event_data, 2);
      char* p = file_receive_buffer;
      size_t num_lines = mpack_node_array_length(filecontents_node);
      // printf("num_lines: %zu\n", num_lines);
      for (size_t i = 0; i < num_lines; ++i) {
        mpack_node_t line = mpack_node_array_at(filecontents_node, i);
        size_t len = mpack_node_strlen(line);
        memcpy(p, mpack_node_str(line), len);
        p[len] = '\n';
        p += len + 1;
      }
      *p = 0;
      callback(name_copy, file_receive_buffer);
      MPACK_FREE(name_copy);
    }
  }
}

bool nvim_connection_poll(void (*file_update)(char* name, char* contents)) {
  bool ok = mpack_tree_try_parse(&nvim_tree);
  mpack_error_t err = mpack_tree_error(&nvim_tree);
  if (err != mpack_ok) {
    fprintf(stderr, "got error %d\n", err);
    return false;
  }
  if (ok) {
    got_message(mpack_tree_root(&nvim_tree), file_update);
  }
  return true;
}

bool nvim_connection_send_quit_and_shutdown(HANDLE pipe) {
  char* data;
  size_t size;
  mpack_writer_t writer;
  mpack_writer_init_growable(&writer, &data, &size);

  mpack_build_array(&writer);
  mpack_write_u32(&writer, 0);  // "Request"
  mpack_write_u32(&writer, ++message_id);
  mpack_write_cstr(&writer, "nvim_command");
  mpack_build_array(&writer);
  mpack_write_cstr(&writer, ":wqall");
  mpack_complete_array(&writer);
  mpack_complete_array(&writer);

  if (mpack_writer_destroy(&writer) != mpack_ok) {
    fprintf(stderr, "An error occurred encoding the data!\n");
    return false;
  }

  send_message_and_free(pipe, data, size);

  // This is some hot garbage. I don't think the flush does anything (the docs
  // imply it's for the server end only). I also tried FILE_FLAG_WRITE_THROUGH
  // and FILE_FLAG_NO_BUFFERING but I wasn't able to get a reliable quit of nvim
  // when the handle is closed here without the Sleep(). ("reliable") TBD: Maybe
  // nvim is discarding the ex command because the client handle is closed by
  // the time it's serviced?
  //
  // As the failure case is just that you have to close nvim manually, we'll
  // tolerate the Sleep for now.
  FlushFileBuffers(pipe);
  Sleep(100);
  CloseHandle(pipe);

  return true;
}
