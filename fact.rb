

def fact (n)
  tot = 1

  for i in 1..n do
    tot = tot * i
  end

  tot
end

start_time = Time.now
res = fact(100000)
end_time = Time.now
resstr = "#{res}"
len = resstr.length
cnt = 30
pos2 = len - cnt
digits1 = resstr[0,cnt]
digits2 = resstr[pos2,len]
puts("#{digits1}...<#{len - (2 * cnt)} digits>...#{digits2}")
puts("#{end_time - start_time} seconds of real time")
