function throw()
   local x = math.random()
   local y = math.random()

   return x*x + y*y <= 1.0
end

function pi(throws)
   local hits = 0
   for i = 1,throws do
      if throw() then hits = hits + 1 end
   end

   return 4 * hits / throws
end

math.randomseed(os.time())
print(pi(100000000))
