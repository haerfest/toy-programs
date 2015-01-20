function answer(limit)
   local set = {}

   for a = 2,limit do
      for b = 2,limit do
         value = tostring(a^b)  -- 64-bit doubles have 52-bit precision
         set[value] = true
      end
   end

   count = 0
   for _, _ in pairs(set) do count = count + 1 end

   return count
end

print(answer(100))