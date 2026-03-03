#include "resistor_color_trio.h"
#include <stdio.h>

/*
   resistor_value_t actual =
       color_code((resistor_band_t[]){ ORANGE, ORANGE, BLACK });
   TEST_ASSERT_EQUAL_UINT16(33, actual.value);
   TEST_ASSERT_EQUAL(OHMS, actual.unit);
}
*/

// 0 -- 999 ohms
// 1 -- 999 kiloohms
// 1 -- 999 megaohms
// 1 -- 999 gigaohms
resistor_value_t color_code(resistor_band_t band[]);
resistor_value_t color_code(resistor_band_t band[]) {
  int value = band[0] * 10 + band[1];
  switch (band[2]) {
    case BLACK:
      return (resistor_value_t) { .value = value, .unit = OHMS };
    case BROWN:
      return (resistor_value_t) { .value = value * 10, .unit = OHMS };
    case RED: // 20 00
      if (value > 9) {
        return (resistor_value_t) { .value = value / 10, .unit = KILOOHMS };
      } else {
        return (resistor_value_t) { .value = value * 100, .unit = OHMS };
      };
    case ORANGE:
      return (resistor_value_t) { .value = value, .unit = KILOOHMS };
    case YELLOW:
      return (resistor_value_t) { .value = value * 10, .unit = KILOOHMS };
    case GREEN:
      if (value > 9) {
        return (resistor_value_t) { .value = value / 10, .unit = MEGAOHMS };
      } else {
        return (resistor_value_t) { .value = value * 100, .unit = KILOOHMS };
      };
    case BLUE:
      return (resistor_value_t) { .value = value, .unit = MEGAOHMS };
    case VIOLET:
      return (resistor_value_t) { .value = value * 10, .unit = MEGAOHMS };
    case GREY:
      if (value > 9) {
        return (resistor_value_t) { .value = value / 10, .unit = GIGAOHMS };
      } else {
        return (resistor_value_t) { .value = value * 100, .unit = MEGAOHMS };
      };
    case WHITE:
      return (resistor_value_t) { .value = value, .unit = GIGAOHMS };
  default:
      break;
  }
  return (resistor_value_t) { .value = 1, .unit = OHMS };
}

