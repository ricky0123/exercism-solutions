#ifndef RESISTOR_COLOR_TRIO_H
#define RESISTOR_COLOR_TRIO_H

typedef enum {
  OHMS,
  KILOOHMS,
  MEGAOHMS,
  GIGAOHMS
} ohm_unit;

typedef struct {
  int value;
  ohm_unit unit;
} resistor_value_t;

typedef enum {
  BLACK,
  BROWN,
  RED,
  ORANGE,
  YELLOW,
  GREEN,
  BLUE,
  VIOLET,
  GREY,
  WHITE
} resistor_band_t;

resistor_value_t color_code(resistor_band_t band[]);

#endif
