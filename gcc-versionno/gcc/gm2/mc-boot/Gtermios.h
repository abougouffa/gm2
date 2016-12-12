/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/gm2-libs/termios.def.  */


#if !defined (_termios_H)
#   define _termios_H

#ifdef __cplusplus
extern "C" {
#endif
#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GSYSTEM.h"

#   if defined (_termios_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif

typedef void *termios_TERMIOS;

typedef enum {termios_vintr, termios_vquit, termios_verase, termios_vkill, termios_veof, termios_vtime, termios_vmin, termios_vswtc, termios_vstart, termios_vstop, termios_vsusp, termios_veol, termios_vreprint, termios_vdiscard, termios_vwerase, termios_vlnext, termios_veol2} termios_ControlChar;

typedef enum {termios_ignbrk, termios_ibrkint, termios_ignpar, termios_iparmrk, termios_inpck, termios_istrip, termios_inlcr, termios_igncr, termios_icrnl, termios_iuclc, termios_ixon, termios_ixany, termios_ixoff, termios_imaxbel, termios_opost, termios_olcuc, termios_onlcr, termios_ocrnl, termios_onocr, termios_onlret, termios_ofill, termios_ofdel, termios_onl0, termios_onl1, termios_ocr0, termios_ocr1, termios_ocr2, termios_ocr3, termios_otab0, termios_otab1, termios_otab2, termios_otab3, termios_obs0, termios_obs1, termios_off0, termios_off1, termios_ovt0, termios_ovt1, termios_b0, termios_b50, termios_b75, termios_b110, termios_b135, termios_b150, termios_b200, termios_b300, termios_b600, termios_b1200, termios_b1800, termios_b2400, termios_b4800, termios_b9600, termios_b19200, termios_b38400, termios_b57600, termios_b115200, termios_b240400, termios_b460800, termios_b500000, termios_b576000, termios_b921600, termios_b1000000, termios_b1152000, termios_b1500000, termios_b2000000, termios_b2500000, termios_b3000000, termios_b3500000, termios_b4000000, termios_maxbaud, termios_crtscts, termios_cs5, termios_cs6, termios_cs7, termios_cs8, termios_cstopb, termios_cread, termios_parenb, termios_parodd, termios_hupcl, termios_clocal, termios_lisig, termios_licanon, termios_lxcase, termios_lecho, termios_lechoe, termios_lechok, termios_lechonl, termios_lnoflsh, termios_ltopstop, termios_lechoctl, termios_lechoprt, termios_lechoke, termios_lflusho, termios_lpendin, termios_liexten} termios_Flag;

EXTERN termios_TERMIOS termios_InitTermios (void);
EXTERN termios_TERMIOS termios_KillTermios (termios_TERMIOS t);
EXTERN int termios_cfgetospeed (termios_TERMIOS t);
EXTERN int termios_cfgetispeed (termios_TERMIOS t);
EXTERN int termios_cfsetospeed (termios_TERMIOS t, unsigned int b);
EXTERN int termios_cfsetispeed (termios_TERMIOS t, unsigned int b);
EXTERN int termios_cfsetspeed (termios_TERMIOS t, unsigned int b);
EXTERN int termios_tcgetattr (int fd, termios_TERMIOS t);
EXTERN int termios_tcsnow (void);
EXTERN int termios_tcsdrain (void);
EXTERN int termios_tcsflush (void);
EXTERN int termios_tcsetattr (int fd, int option, termios_TERMIOS t);
EXTERN void termios_cfmakeraw (termios_TERMIOS t);
EXTERN int termios_tcsendbreak (int fd, int duration);
EXTERN int termios_tcdrain (int fd);
EXTERN int termios_tcflushi (int fd);
EXTERN int termios_tcflusho (int fd);
EXTERN int termios_tcflushio (int fd);
EXTERN int termios_tcflowoni (int fd);
EXTERN int termios_tcflowoffi (int fd);
EXTERN int termios_tcflowono (int fd);
EXTERN int termios_tcflowoffo (int fd);
EXTERN unsigned int termios_GetFlag (termios_TERMIOS t, termios_Flag f, unsigned int *b);
EXTERN unsigned int termios_SetFlag (termios_TERMIOS t, termios_Flag f, unsigned int b);
EXTERN unsigned int termios_GetChar (termios_TERMIOS t, termios_ControlChar c, char *ch);
EXTERN unsigned int termios_SetChar (termios_TERMIOS t, termios_ControlChar c, char ch);
#ifdef __cplusplus
}
#endif

#   undef EXTERN
#endif
