function powerToDigits (n, p) {
  var digits = [1],
      carry  = 0;

  for (var i = 0; i < p; i++) {
    for (var j = 0; j < digits.length; j++) {
      digits[j] = digits[j] * n + carry;
      carry = 0;

      if (digits[j] >= 10) {
        digits[j] -= 10;
        
        if (j == digits.length - 1) {
          digits[j + 1] = 1;
          break;
        }

        carry = 1;
      }
    }
  }

  return digits;
}

function sumDigits (digits) {
  var sum = 0;

  for (var i = 0; i < digits.length; i++) {
    sum += digits[i];
  }

  return sum;
}

console.log(sumDigits(powerToDigits(2, 1000)));
