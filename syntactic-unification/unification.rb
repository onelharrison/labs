# frozen_string_literal: true

class Var
  attr_reader :name

  def initialize(name)
    @name = name
  end

  def inspect
    "#{self.class}(name: #{@name.inspect})"
  end
end

def unify(a, b, model = {})
  a = walk(a, model)
  b = walk(b, model)

  return model if a == b

  if a.is_a?(Var)
    return if occurs?(a, b, model)

    return model.merge({ a => b })
  end

  if b.is_a?(Var)
    return if occurs?(a, b, model)

    return model.merge({ b => a })
  end

  if a.is_a?(Array) && b.is_a?(Array)
    return None unless a.length == b.length

    a.zip(b).each do |ax, bx|
      model = unify(ax, bx, model)
      return nil if model.nil?
    end

    return model
  end

  nil
end

def walk(term, model)
  term = model[term] while model.key?(term)
  term
end

def occurs?(a, b, model)
  b = walk(b, model)
  return true if b == a
  return b.any? { |bx| occurs?(a, bx, model) } if b.is_a?(Array)

  false
end
