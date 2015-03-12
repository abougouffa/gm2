/* this needs to be maintained */
%module pgeif

%include exception.i

%exception {
  try {
     $action
  } catch (int i) {
     return NULL;
  }
}

%include cstring.i
%cstring_output_allocate_size(char **s, int *slen, );

%{

extern "C" void get_ebuf(char **s, int *slen);
extern "C" void get_fbuf(char **s, int *slen);

extern "C" void empty_buffer (void);
extern "C" void batch (void);
extern "C" void use_buffer (void);
extern "C" void use_time_delay (unsigned int on) ;
extern "C" unsigned int rm (unsigned int id);
extern "C" double get_time (void);
extern "C" double time_until (void);
extern "C" unsigned int is_frame (void);
extern "C" unsigned int is_collision (void);
extern "C" void process_event (void);
extern "C" unsigned int rotate (unsigned int id, double angle);
extern "C" unsigned int accel (unsigned int id, double ax, double ay);
extern "C" unsigned int velocity (unsigned int id, double vx, double vy);
extern "C" unsigned int circle (double x0, double y0, double radius, unsigned int c);
extern "C" unsigned int fix (unsigned int id);
extern "C" unsigned int mass (unsigned int id, double m);
extern "C" unsigned int poly6 (double x0, double y0, double x1, double y1, double x2, double y2, double x3, double y3, double x4, double y4, double x5, double y5, unsigned int c);
extern "C" unsigned int poly5 (double x0, double y0, double x1, double y1, double x2, double y2, double x3, double y3, double x4, double y4, unsigned int c);
extern "C" unsigned int poly4 (double x0, double y0, double x1, double y1, double x2, double y2, double x3, double y3, unsigned int c);
extern "C" unsigned int poly3 (double x0, double y0, double x1, double y1, double x2, double y2, unsigned int c);
extern "C" unsigned int box (double x0, double y0, double i, double j, unsigned int c);
extern "C" void gravity (double g);
extern "C" unsigned int purple (void);
extern "C" unsigned int blue (void);
extern "C" unsigned int green (void);
extern "C" unsigned int red (void);
extern "C" unsigned int black (void);
extern "C" unsigned int white (void);
extern "C" unsigned int rgb (double r, double g, double b);
%}

extern "C" void empty_buffer (void);
extern "C" void batch (void);
extern "C" void use_buffer (void);
extern "C" void use_time_delay (unsigned int on) ;
extern "C" unsigned int rm (unsigned int id);
extern "C" double get_time (void);
extern "C" double time_until (void);
extern "C" unsigned int is_frame (void);
extern "C" unsigned int is_collision (void);
extern "C" void process_event (void);
extern "C" unsigned int rotate (unsigned int id, double angle);
extern "C" unsigned int accel (unsigned int id, double ax, double ay);
extern "C" unsigned int velocity (unsigned int id, double vx, double vy);
extern "C" unsigned int circle (double x0, double y0, double radius, unsigned int c);
extern "C" unsigned int fix (unsigned int id);
extern "C" unsigned int mass (unsigned int id, double m);
extern "C" unsigned int poly6 (double x0, double y0, double x1, double y1, double x2, double y2, double x3, double y3, double x4, double y4, double x5, double y5, unsigned int c);
extern "C" unsigned int poly5 (double x0, double y0, double x1, double y1, double x2, double y2, double x3, double y3, double x4, double y4, unsigned int c);
extern "C" unsigned int poly4 (double x0, double y0, double x1, double y1, double x2, double y2, double x3, double y3, unsigned int c);
extern "C" unsigned int poly3 (double x0, double y0, double x1, double y1, double x2, double y2, unsigned int c);
extern "C" unsigned int box (double x0, double y0, double i, double j, unsigned int c);
extern "C" void gravity (double g);
extern "C" unsigned int purple (void);
extern "C" unsigned int blue (void);
extern "C" unsigned int green (void);
extern "C" unsigned int red (void);
extern "C" unsigned int black (void);
extern "C" unsigned int white (void);
extern "C" unsigned int rgb (double r, double g, double b);

extern "C" void get_ebuf (char **s, int *slen);
extern "C" void get_fbuf (char **s, int *slen);
