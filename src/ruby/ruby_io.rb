class RubyIO
  def puts(str)
    i = 0
    while i < str.size
      putc str[i]
      i += 1
    end
    putc 10
  end
end
