import java.util.Random;

import nilgiri.math.*;
import nilgiri.math.autodiff.*;
import nilgiri.math.ia.*;

public class Main {
    private final DoubleRealFactory RNFactory = DoubleRealFactory.instance();
    private final DifferentialRealFunctionFactory<DoubleReal> DFFactory = new DifferentialRealFunctionFactory<DoubleReal>(RNFactory);

    private static boolean doublesEqual(double a, double b) {
        return Math.abs(a - b) < 1e-10 * (Math.abs(a) + 1e-3); // proportional error
    }

    private void test(double i_expected, DifferentialFunction<DoubleReal> i_f){
        String func_str = i_f.toString();
        double func_value = i_f.getValue().doubleValue();

        System.out.println(func_str + " = " + func_value + " == " + i_expected);
        if (!doublesEqual(func_value, i_expected)) {
            throw new RuntimeException("Test failed: " + func_value + " != " + i_expected);
        }
    }

    public void testFunc(double vx, double vy, double vq, double testVx, double testVy) {
        Variable<DoubleReal> x = DFFactory.var("x", new DoubleReal(vx));
        Variable<DoubleReal> y = DFFactory.var("y", new DoubleReal(vy));
        Constant<DoubleReal> q = DFFactory.val(new DoubleReal(vq));

        //================================================================================
        //Construct functions
        //================================================================================
        //h = q*x*( cos(x*y) + y )
        DifferentialFunction<DoubleReal> h = q.mul(x).mul( DFFactory.cos( x.mul(y) ).plus(y) );

        //ph/px = q*( cos(x*y) + y ) + q*x*( -sin(x*y)*y )
        DifferentialFunction<DoubleReal> dhpx = h.diff(x);

        //ph/py = q*x*( -sin(x*y)*x + 1.0)
        DifferentialFunction<DoubleReal> dhpy = h.diff(y);

        //p2h/px2 = q*( -sin(x*y)*y + y ) + q*( -sin(x*y)*y ) + q*x*( -cos(x*y)*y*y )
        DifferentialFunction<DoubleReal> d2hpxpx = dhpx.diff(x);

        //p2h/pypx = q*( -sin(x*y)*x + 1.0 ) + q*x*( -sin(x*y) - cos(x*y)*y*y )
        DifferentialFunction<DoubleReal> d2hpypx = dhpx.diff(y);

        //================================================================================
        //Test functions { h, ph/px, ph/py, p2h/px2, p2h/pypx }.
        //================================================================================
        test(vq*vx*( Math.cos(vx*vy) + vy ), h);
        test(vq*( Math.cos(vx*vy) + vy ) + vq*vx*(-Math.sin(vx*vy)*vy ), dhpx);
        test(vq*vx*( -Math.sin(vx*vy)*vx + 1.0 ), dhpy);
        test(vq*( -Math.sin(vx*vy)*vy ) + vq*( -Math.sin(vx*vy)*vy ) + vq*vx*(-Math.cos(vx*vy)*vy*vy), d2hpxpx);
        test(vq*( -Math.sin(vx*vy)*vx + 1.0 ) + vq*vx*( -Math.sin(vx*vy) - Math.cos(vx*vy)*vx*vy ), d2hpypx);

        //================================================================================
        //Change values of the variables.
        //================================================================================
        vx = testVx;
        vy = testVy;
        x.set(new DoubleReal(vx));
        y.set(new DoubleReal(vy));

        //================================================================================
        //Re-Test functions { h, ph/px, ph/py, p2h/px2, p2h/pypx }.
        //================================================================================
        //No reconstruction of the functions is necessary
        //to get values of the functions for new values of variables.
        test(vq*vx*( Math.cos(vx*vy) + vy ), h);
        test(vq*( Math.cos(vx*vy) + vy ) + vq*vx*(-Math.sin(vx*vy)*vy ), dhpx);
        test(vq*vx*( -Math.sin(vx*vy)*vx + 1.0 ), dhpy);
        test(vq*( -Math.sin(vx*vy)*vy ) + vq*( -Math.sin(vx*vy)*vy ) + vq*vx*(-Math.cos(vx*vy)*vy*vy), d2hpxpx);
        test(vq*( -Math.sin(vx*vy)*vx + 1.0 ) + vq*vx*( -Math.sin(vx*vy) - Math.cos(vx*vy)*vx*vy ), d2hpypx);
    }

    private static final double MAGNITUDE = 100;

    public static void main(String[] args) {
        Main tester = new Main();
        tester.testFunc(3.0, 5.0, 8.0, 4.0, 7.0);
        tester.testFunc(1.0, 2.0, 3.0, 4.0, 5.0);
        tester.testFunc(-1.0, -2.0, -3.0, -4.0, -5.0);

        Random random = new Random(0xC0DE_BEEF);
        for(int i = 0; i < 1000; i++){
            double vx = random.nextDouble() * MAGNITUDE;
            double vy = random.nextDouble() * MAGNITUDE;
            double vq = random.nextDouble() * MAGNITUDE;
            double testVx = random.nextDouble() * MAGNITUDE;
            double testVy = random.nextDouble() * MAGNITUDE;
            tester.testFunc(vx, vy, vq, testVx, testVy);
        }
    }

}