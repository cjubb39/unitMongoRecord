class Unit
  include Mongoid::Document
  field :Fluid, type: String
  field :Group, type: String
  field :SN, type: Integer
  field :chNull, type: Float
  field :chTCR, type: Float
  field :crNull, type: Float
  field :crTCR, type: Float
  field :fullSN, type: String
  field :hyst, type: Float
  field :lin, type: Float
  field :nfsoSet, type: Float
  field :nullSet, type: Float
  field :rhNull, type: Float
  field :rhTCR, type: Float

  def self.search(search)
    if search
      Unit.any_of({Fluid: /#{Regexp.escape(search)}/}, \
        {Group: /#{Regexp.escape(search)}/}, \
        {fullSN: /#{Regexp.escape(search)}/})
    else
      scoped
    end
  end
end
