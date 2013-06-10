#!/usr/bin/env ruby


class Camera
  attr_reader :x, :y, :z, :dir

  def initialize(x, y, z, dir = :left)
    @x   = x
    @y   = y
    @z   = z
    @dir = dir
  end

  def project(v)
    r = Rectangle.new

    r.left_x   = v.x - v.length / 2
    r.right_x  = v.x + v.length / 2
    r.top_y    = v.y + v.width  / 2
    r.bottom_y = v.y - v.width  / 2

    r.left_x   = project_x(v, r.left_x)   if r.left_x   < @x
    r.right_x  = project_x(v, r.right_x)  if r.right_x  > @x
    r.top_y    = project_y(v, r.top_y)    if r.top_y    > @y
    r.bottom_y = project_y(v, r.bottom_y) if r.bottom_y < @y

    return r
  end

  private

  def project_x(v, x)
    dx = x - @x
    return x + (dx * v.height) / (@z - v.height)
  end

  def project_y(v, y)
    dy = y - @y
    return y + (dy * v.height) / (@z - v.height)
  end
end


class Rectangle
  attr_accessor :left_x, :right_x, :top_y, :bottom_y

  def to_s(road = nil)
    q = road ? road.pixels(self) : self
    "%d %d %d %d " % [100 * q.left_x, 100 * q.right_x, 100 * q.top_y, 100 * q.bottom_y]
  end
end


class Road
  attr_reader :left_x, :right_x, :top_y, :bottom_y

  def initialize(left_x, right_x, top_y, bottom_y, ppm = 20.0)
    @left_x   = left_x
    @right_x  = right_x
    @top_y    = top_y
    @bottom_y = bottom_y
    @ppm      = ppm
  end

  def limit(r)
    return nil if r.right_x  < @left_x
    return nil if r.left_x   > @right_x
    return nil if r.top_y    < @bottom_y
    return nil if r.bottom_y > @top_y

    p = Rectangle.new

    p.left_x   = [r.left_x,   @left_x].max
    p.right_x  = [r.right_x,  @right_x].min
    p.top_y    = [r.top_y,    @top_y].min
    p.bottom_y = [r.bottom_y, @bottom_y].max

    return p
  end

  def pixels(r)
    return nil unless r

    p = Rectangle.new

    p.left_x   = (r.left_x   - @left_x)   * @ppm
    p.right_x  = (r.right_x  - @left_x)   * @ppm
    p.top_y    = (r.top_y    - @top_y)    * @ppm
    p.bottom_y = (r.bottom_y - @bottom_y) * @ppm

    return p
  end
end


class Vehicle
  attr_reader :length, :width, :height, :x, :y

  def initialize(length, width, height, dir = :right)
    @length       = length
    @width        = width
    @height       = height
    @dir          = dir
    @velocity     = 0.0
    @acceleration = 0.0
    @limit        = nil
    @actions      = {}
    @time         = nil
  end

  def place(t, x, y, v = 0.0, a = 0.0)
    @actions[t] = { action: :place, x: x, y: y, v: v, a: a, started?: false }
  end

  def accelerate(t, a, to)
    @actions[t] = { action: :accelerate, a: a, to: to, started?: false }
  end

  def brake(t, a, to)
    @actions[t] = { action: :accelerate, a: -a, to: to, started?: false }
  end

  def start(t)
    @actions.each { |k, v| v[:started?] = false }
    @time = t
  end

  def act(t)
    time = @actions.keys.take_while { |time| time <= t }.last
    raise "No action for vehicle at time #{t}" unless time

    perform(@actions[time]) unless @actions[time][:started?]
    
    update(t)
    
    @time = t
  end

  private

  def perform(action)
    if action[:action] == :place
      @x            = action[:x]
      @y            = action[:y]
      @acceleration = action[:a]
      @velocity     = action[:v] / 3.6
      @limit        = nil
    elsif action[:action] == :accelerate
      @acceleration = action[:a]
      @limit        = action[:to]      
    end

    action[:started?] = true    
  end

  def update(t)    
    dt = t - @time

    unless @acceleration == 0
      @velocity += @acceleration * dt 
      @velocity = @limit if @limit and ((@acceleration > 0 and @velocity > @limit) or (@acceleration < 0 and @velocity < @limit))
    end

    unless @velocity == 0
      @x = case @dir
        when :left  then @x - @velocity * dt
        when :right then @x + @velocity * dt
      end
    end
  end

end


class Scenario
  attr_accessor :camera, :road, :vehicles

  def initialize(combine_dist = 2.7)
    @combine_dist = combine_dist
    @camera       = nil
    @road         = nil
    @vehicles     = []
  end

  def play(duration, t = 0)
    abort 'No camera defined yet'   unless @camera
    abort 'No road defined yet'     unless @road
    abort 'No vehicles defined yet' unless @vehicles.count > 0

    @vehicles.each { |v| v.start(t) }
    
    t.step(t + duration, 0.033) do |t|
      rects = @vehicles.reduce([]) do |acc, v|
        v.act(t)
        if r = noisify(@road.limit(@camera.project(v)))
          if combined?(acc, r)
            acc[-1] = combine(acc, r)
          else
            acc << r
          end
        end
        acc
      end

      print "%.2f %d " % [t, rects.count]
      rects.each { |r| print r.to_s }
      puts
    end
  end

  private

  def noisify(r, level = 0.5)
    return nil unless r

    f = 1E3

    p = Rectangle.new
    p.left_x   = r.left_x   + rand(f * 2 * level + 1) / f - level
    p.right_x  = r.right_x  + rand(f * 2 * level + 1) / f- level
    p.top_y    = r.top_y    + rand(f * 2 * level + 1) / f - level
    p.bottom_y = r.bottom_y + rand(f * 2 * level + 1) / f - level

    return p
  end

  def combined?(rects, r)
    return false if rects.empty?
    
    prev = rects.last

    dx = [prev.left_x,   r.left_x].max   - [prev.right_x, r.right_x].min
    dy = [prev.bottom_y, r.bottom_y].max - [prev.top_y,   r.top_y].min

    dx.abs <= @combine_dist and dy.abs <= @combine_dist
  end

  def combine(rects, r)
    combined = Rectangle.new

    combined.left_x   = [rects.last.left_x,   r.left_x].min
    combined.right_x  = [rects.last.right_x,  r.right_x].max
    combined.top_y    = [rects.last.top_y,    r.top_y].max
    combined.bottom_y = [rects.last.bottom_y, r.bottom_y].min

    return combined
  end
end


s = Scenario.new
s.camera = Camera.new(0, +7.5, +6.0, :left)
s.road   = Road.new(-21.5, +2.5, +9.0, +6.0)

peu208 = Vehicle.new(3.962, 1.739, 1.460, :right)
s.vehicles << peu208

megane = Vehicle.new(4.498, 1.777, 1.457, :right)
s.vehicles << megane

peu208.place(0, -25.0, +7.5, 20)
peu208.brake(3, 6.0, 0)
peu208.accelerate(6, 1.5, 50)

megane.place(0, -37.0, +7.0, 20)
megane.brake(3.5, 5.0, 0)
megane.accelerate(7, 1.8, 45)

s.play(10.0)
