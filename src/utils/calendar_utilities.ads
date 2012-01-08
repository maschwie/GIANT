--  taken from: http://groups.google.com/groups?hl=en&lr=&ie=UTF-8&oe=UTF-8&threadm=EMERY.90Jul6094911%40aries.linus.mitre.org&rnum=1&prev=/groups%3Fhl%3Den%26lr%3D%26ie%3DUTF-8%26oe%3DUTF-8%26q%3Dada%2Bduration%2Bformat%26btnG%3DGoogle%2BSearch

--  copyright (taken from posting in newsgroup)
--
--  Here's a package that might prove useful in doing those tricky date
--  calculations in Ada.  As usual, if any bugs in this code are caught or
--  captured, I disavow any knowledge of (and responsibility for) their
--  actions.
--
--    dave emery
--    emery@aries.mitre.org

with Calendar;
package Calendar_Utilities is

  subtype hours_number is integer range 0..23;
  subtype minutes_number is integer range 0..59;
  subtype seconds_number is integer range 0..59;
  subtype decimal_seconds is duration range duration'(0.0) .. duration'(1.0);

  -- date strings are of the form
  -- year/mo/da hr:mn:sc.decimal

  function image_of (time : Calendar.time)
      return string;

  function value_of (str : string)
      return Calendar.time;

  function image_size (time : Calendar.time)
      return natural;

  function years_in (time : Calendar.time)
      return Calendar.year_number;

  function months_in (time : Calendar.time)
      return Calendar.month_number;

  function days_in (time : Calendar.time)
      return Calendar.day_number;

  function seconds_in (time : Calendar.time)
      return Calendar.day_duration;

  -- duration strings are of the form
  -- hr:mn:sc.decimal

  function image_of (d : duration)
      return string;

  function value_of (str : string)
      return duration;

  function image_size (d : duration)
      return natural;

  procedure split (d   : in     duration;
       hr  :    out hours_number;
     min :    out minutes_number;
     sec :    out seconds_number;
     dec :    out decimal_seconds);

  function duration_of (hrs : hours_number;
     min : minutes_number;
   sec : seconds_number;
   dec : decimal_seconds)
      return duration;

  function hours_in (d : duration)
      return hours_number;

  function minutes_in (d : duration)
      return minutes_number;

  function seconds_in (d : duration)
      return seconds_number;

  function decimal_seconds_in (d : duration)
      return decimal_seconds;

  function clock_string
      return string; -- is image_of(Calendar.clock);

end Calendar_Utilities;