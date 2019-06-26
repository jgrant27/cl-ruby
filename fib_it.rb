

def fib(n)
  curr = 0
  succ = 1
  presucc = nil

  for i in 1..n do
    presucc = succ
    succ = curr + succ
    curr = presucc
  end

  curr
end

start_time = Time.now
res = fib(1000000)
end_time = Time.now
resstr = "#{res}"
len = resstr.length
cnt = 30
pos2 = len - cnt
digits1 = resstr[0,cnt]
digits2 = resstr[pos2,len]
puts("#{digits1}...<#{len - (2 * cnt)} digits>...#{digits2}")
puts("#{end_time - start_time} seconds of real time")

