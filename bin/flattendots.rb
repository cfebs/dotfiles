#!/usr/bin/env ruby
# Flattens dotfiles that used to be tracked in ansible
# Use env vars to do things:
# - DOIT: set to 1 for no-dry run
out = `find . -maxdepth 5 -type l 2>/dev/null`

doit = !ENV["DOIT"].nil?

out.split.each do |l|
  real_path = ''
  begin
    real_path = File.readlink(l)
  rescue
  end
  if !real_path
    next
  end

  if real_path.include?('pacman-bar')
    next
  end

  if real_path.include?('arch-i3-desktop')
    next
  end

  if real_path.include?('ansibles')
    puts real_path
    if doit
      puts 'copying'
      `cp --remove-destination #{real_path} #{l}`
      `git add -f #{l}`
    end
  end
end
