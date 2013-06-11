require 'test_helper'

class UnitsControllerTest < ActionController::TestCase
  setup do
    @unit = units(:one)
  end

  test "should get index" do
    get :index
    assert_response :success
    assert_not_nil assigns(:units)
  end

  test "should get new" do
    get :new
    assert_response :success
  end

  test "should create unit" do
    assert_difference('Unit.count') do
      post :create, unit: { Fluid: @unit.Fluid, Group: @unit.Group, SN: @unit.SN, chNull: @unit.chNull, chTCR: @unit.chTCR, crNull: @unit.crNull, crTCR: @unit.crTCR, fullSN: @unit.fullSN, hyst: @unit.hyst, lin: @unit.lin, nfsoSet: @unit.nfsoSet, nullSet: @unit.nullSet, rhNull: @unit.rhNull, rhTCR: @unit.rhTCR }
    end

    assert_redirected_to unit_path(assigns(:unit))
  end

  test "should show unit" do
    get :show, id: @unit
    assert_response :success
  end

  test "should get edit" do
    get :edit, id: @unit
    assert_response :success
  end

  test "should update unit" do
    put :update, id: @unit, unit: { Fluid: @unit.Fluid, Group: @unit.Group, SN: @unit.SN, chNull: @unit.chNull, chTCR: @unit.chTCR, crNull: @unit.crNull, crTCR: @unit.crTCR, fullSN: @unit.fullSN, hyst: @unit.hyst, lin: @unit.lin, nfsoSet: @unit.nfsoSet, nullSet: @unit.nullSet, rhNull: @unit.rhNull, rhTCR: @unit.rhTCR }
    assert_redirected_to unit_path(assigns(:unit))
  end

  test "should destroy unit" do
    assert_difference('Unit.count', -1) do
      delete :destroy, id: @unit
    end

    assert_redirected_to units_path
  end
end
