/***************************  vsectionmath_common.h   ****************************
* Author:        w
* Date created:  2016-10-10
* Last modified: 2016-10-12
* Version:       0.02
* Project:       vector struct
* Description:
* Header file containing common code for inline version of mathematical functions.
*
* Theory, methods and inspiration based partially on these sources:
* > Moshier, Stephen Lloyd Baluk: Methods and programs for mathematical functions.
*   Ellis Horwood, 1989.
* > VDT library developed on CERN by Danilo Piparo, Thomas Hauth and
*   Vincenzo Innocente, 2012, https://svnweb.cern.ch/trac/vdt
* > Cephes math library by Stephen L. Moshier 1992,
*   http://www.netlib.org/cephes/
*
* Calculation methods:
* Some functions are using Padé approximations f(x) = P(x)/Q(x)
* Most single precision functions are using Taylor expansions
*
*
* (c) Copyright 2014-2016 GNU General Public License http://www.gnu.org/licenses
******************************************************************************/

#ifndef VSECTIONMATH_COMMON_H
#define VSECTIONMATH_COMMON_H  1

#ifdef VECTORMATH_LIB_H
#error conflicting header files: vectormath_lib.h for external math functions, other vectormath_xxx.h for inline math functions
#endif

#include <math.h>

// Maximum vector size, bits. Allowed values are 128, 256, 512
#ifndef MAX_VECTOR_SIZE
#define MAX_VECTOR_SIZE 256
#endif

#if INSTRSET < 2             // SSE2 required
  #error Please compile for the SSE2 instruction set or higher
#else
#endif  // INSTRSET < 2


/******************************************************************************
               define mathematical constants
******************************************************************************/
#define VM_PI       3.14159265358979323846           // pi
#define VM_PI_2     1.57079632679489661923           // pi / 2
#define VM_PI_4     0.785398163397448309616          // pi / 4
#define VM_SQRT2    1.41421356237309504880           // sqrt(2)
#define VM_LOG2E    1.44269504088896340736           // 1/log(2)
#define VM_LOG10E   0.434294481903251827651          // 1/log(10)
#define VM_LN2      0.693147180559945309417          // log(2)
#define VM_LN10     2.30258509299404568402           // log(10)
#define VM_SMALLEST_NORMAL  2.2250738585072014E-308  // smallest normal number, double
#define VM_SMALLEST_NORMALF 1.17549435E-38f          // smallest normal number, float

#endif

