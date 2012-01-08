with Text_IO;
package body Calendar_Utilities is

  use Calendar;

  function two_digit_image (n : natural)
      return string;
  pragma inline (two_digit_image);

  function to_integer (d : duration)
      return integer;
  pragma inline (to_integer);

  no_number_found : exception;

  procedure get_next_number
 (str : string;
  start : natural;
  value : out integer;
  next_char : out natural);
  pragma inline (get_next_number);

  function image_of (time : Calendar.time)
      return string
  is
    year : year_number;
    month : month_number;
    day : day_number;
    seconds : day_duration;
  begin
     --  changed 20030831 by koppor from '/' to '-'
    split (time, year, month, day, seconds);
    return integer'image(year)(2..5)
    & '-' & two_digit_image(month)
    & '-' & two_digit_image(day)
    & ' ' & image_of(seconds);
  end image_of;

  function value_of (str : string)
      return Calendar.time
  is
    answer : Calendar.time;
    year : year_number;
    month : month_number;
    day : day_number;
    seconds : day_duration;
    start : natural := str'first;
  begin  -- year/mo/da hr:mn:sc.decimal
    -- find year
    get_next_number (str, start, year, start);
    if (str(start) /= '/') then
      raise constraint_error; -- format error
    else
      start := start + 1;
    end if;

    -- find month
    get_next_number (str, start, month, start);
    if (str(start) /= '/') then
      raise constraint_error; -- format error
    else
      start := start + 1;
    end if;

    -- find day
    get_next_number (str, start, day, start);

    -- get seconds
    seconds := value_of (str(start..str'last));

    answer := Calendar.time_of (year, month, day, seconds);
    return answer;
  end value_of;

  function image_size (time : Calendar.time)
      return natural
  is
    year : year_number;
    month : month_number;
    day : day_number;
    seconds : day_duration;
  begin
    Calendar.split (time, year, month, day, seconds);
 -- year /   mo  /   da      hr:mn:sc.decimal
    return 4 +  1 + 2 + 1 + 2 + 1 + image_size(seconds);
  end image_size;

  function years_in (time : Calendar.time)
      return Calendar.year_number
  is
    year : year_number;
    month : month_number;
    day : day_number;
    seconds : day_duration;
  begin
    Calendar.split (time, year, month, day, seconds);
    return year;
  end years_in;

  function months_in (time : Calendar.time)
      return Calendar.month_number
  is
    year : year_number;
    month : month_number;
    day : day_number;
    seconds : day_duration;
  begin
    Calendar.split (time, year, month, day, seconds);
    return month;
  end months_in;

  function days_in (time : Calendar.time)
      return Calendar.day_number
  is
    year : year_number;
    month : month_number;
    day : day_number;
    seconds : day_duration;
  begin
    Calendar.split (time, year, month, day, seconds);
    return day;
  end days_in;

  function seconds_in (time : Calendar.time)
      return Calendar.day_duration
  is
    year : year_number;
    month : month_number;
    day : day_number;
    seconds : day_duration;
  begin
    Calendar.split (time, year, month, day, seconds);
    return seconds;
  end seconds_in;

  function image_of (d : duration)
      return string
  is
    hr : hours_number;
    min : minutes_number;
    sec : seconds_number;
    dec : decimal_seconds;
    int_decimal : integer;
  begin
    split (d, hr, min, sec, dec);
    int_decimal := to_integer(dec);
    return two_digit_image(hr)
        & ':' & two_digit_image(min)
      & ':' & two_digit_image(sec);
    -- disabled 20030831 by koppor
    -- & "." & integer'image(int_decimal) -- get rid of leading space
    --  (2..integer'image(int_decimal)'last);
  end image_of;

  function value_of (str : string)
      return duration
  is
    hr, min, sec, decimal : integer;
    start : integer := str'first;
    integer_seconds : integer;
    answer : duration;
  begin   -- hr:mn:sc.decimal
    -- find hours
    get_next_number (str, start, hr, start);
    if (str(start) /= ':') then
      raise constraint_error; -- format error
    else
      start := start + 1;
    end if;

    -- find min
    get_next_number (str, start, min, start);
    if (str(start) /= ':') then
      raise constraint_error; -- format error
    else
      start := start + 1;
    end if;

    -- find sec
    get_next_number (str, start, sec, start);
    if (str(start) /= '.') then
      raise constraint_error; -- format error
    else
      start := start + 1;
    end if;

    -- find decimal
    get_next_number (str, start, decimal, start);

    -- put it all together
    integer_seconds := hr * (60*60) + min *60 + sec;
    answer := duration(integer_seconds)
       + duration(decimal * duration(duration'delta));
    return answer;
  end value_of;

  function image_size (d : duration)
      return natural
  is
    decimal_size : integer;
  begin -- hr:mn:sc.decimal
    decimal_size := integer'image
  (to_integer(decimal_seconds_in(d)))'length -1;
    return 2 + 1 + 2 +1 + 2 + 1
    + decimal_size;
  end image_size;

  procedure split (d   : in     duration;
       hr  :    out hours_number;
     min :    out minutes_number;
     sec :    out seconds_number;
     dec :    out decimal_seconds)
  is
    seconds : integer;
    hr_local, min_local : integer;
  begin
    seconds :=integer(d);
    if (duration(seconds) > d) then
 seconds := seconds -1;
    end if;
    dec := d - duration(seconds);
    hr_local:= seconds / (60*60);
    seconds := seconds - (hr_local *(60*60));
    min_local := seconds / 60;
    sec := seconds - (min_local*60);
    hr := hr_local; min := min_local;
  end split;

  function duration_of (hrs : hours_number;
     min : minutes_number;
   sec : seconds_number;
   dec : decimal_seconds)
      return duration
  is
    seconds : integer;
  begin
    seconds := (hrs * (60*60)) + (min * 60) + sec;
    return duration(seconds) + dec;
  end duration_of;

  function hours_in (d : duration)
      return hours_number
  is
    answer : integer;
  begin
    answer := (integer(d) /(60*60));
    return hours_number(answer);
  end hours_in;

  function minutes_in (d : duration)
      return minutes_number
  is
    answer : integer;
  begin
    answer := (integer(d) - (hours_in(d) * 60 * 60)) /60;
    return minutes_number(answer);
  end minutes_in;

  function seconds_in (d : duration)
      return seconds_number
  is
    answer : integer;
  begin
    answer := (integer(d) - (hours_in(d) *60*60) - (minutes_in(d)* 60));
    return seconds_number(answer);
  end seconds_in;

  function decimal_seconds_in (d : duration)
      return decimal_seconds
  is
  begin
    return decimal_seconds(d - duration(integer(d)));
  end decimal_seconds_in;

  function clock_string
      return string -- is image_of(Calendar.clock);
  is
  begin
    return image_of (calendar.clock);
  end clock_string;

  -- private bodies
  function two_digit_image (n : natural)
      return string
  is
  begin
    case n is
      when 0..9 =>
 return '0' & integer'image(n)(2);
      when 10..99 =>
 return integer'image(n)(2..3);
      when others =>
 raise constraint_error;
    end case;
  end two_digit_image;

  function to_integer (d : duration)
      return integer
  is
  begin
    return integer(d * duration(1.0/duration'delta));
  end to_integer;

  procedure get_next_number
 (str : string;
  start : natural;
  value : out integer;
  next_char : out natural)
  is
    current : natural := start;
    answer : integer := 0;
    found_number : boolean := false;
  begin
    loop
      if (current > str'last) then
 exit;
      end if;
      case str(current) is
 when ' ' | Ascii.ht =>
   if found_number then
     exit; -- terminates number
       -- otherwise skip
   end if;
        when '0' .. '9' =>
   found_number := true;
   answer := answer * 10 +
   (character'pos(str(current)) - character'pos('0'));
 when '-' =>  -- negatives not allowed!!
   raise constraint_error;
 when others =>
   exit;
      end case;
      current := current + 1;
    end loop;
    if found_number then
      next_char := current;
      value := answer;
    else
      raise no_number_found;
    end if;
  end get_next_number;


end Calendar_Utilities;
