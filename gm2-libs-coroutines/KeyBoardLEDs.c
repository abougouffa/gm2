
#if defined(linux)

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <linux/kd.h>

#if !defined(TRUE)
#   define TRUE (1==1)
#endif
#if !defined(FALSE)
#   define FALSE (1==0)
#endif


static int fd;
static int initialized = FALSE;


void KeyBoardLEDs_SwitchScroll (int scrolllock)
{
  unsigned char leds;
  int r = ioctl(fd, KDGETLED, &leds);
  if (scrolllock)
    leds = leds | LED_SCR;
  else
    leds = leds & (~ LED_SCR);
  r = ioctl(fd, KDSETLED, leds);
}

void KeyBoardLEDs_SwitchNum (int numlock)
{
  unsigned char leds;
  int r = ioctl(fd, KDGETLED, &leds);
  if (numlock)
    leds = leds | LED_NUM;
  else
    leds = leds & (~ LED_NUM);
  r = ioctl(fd, KDSETLED, leds);
}

void KeyBoardLEDs_SwitchCaps (int capslock)
{
  unsigned char leds;
  int r = ioctl(fd, KDGETLED, &leds);
  if (capslock)
    leds = leds | LED_CAP;
  else
    leds = leds & (~ LED_CAP);
  r = ioctl(fd, KDSETLED, leds);
}

void KeyBoardLEDs_SwitchLeds (int numlock, int capslock, int scrolllock)
{
  KeyBoardLEDs_SwitchScroll(scrolllock);
  KeyBoardLEDs_SwitchNum(numlock);
  KeyBoardLEDs_SwitchCaps(capslock);
}

_M2_KeyBoardLEDs_init (void)
{
  if (! initialized) {
    initialized = TRUE;
    fd = open("/dev/tty", O_RDONLY);
    if (fd == -1) {
      perror("unable to open /dev/tty");
      exit(1);
    }
  }
}

#else
void KeyBoardLEDs_SwitchLeds (int numlock, int capslock, int scrolllock)
{
}

void SwitchScroll (int scrolllock)
{
}

void SwitchNum (int numlock)
{
}

void SwitchCaps (int capslock)
{
}
#endif
