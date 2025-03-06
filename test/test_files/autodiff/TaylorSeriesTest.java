import nilgiri.math.DoubleReal;
import nilgiri.math.DoubleRealFactory;
import nilgiri.math.autodiff.DifferentialFunction;
import nilgiri.math.autodiff.DifferentialRealFunctionFactory;
import nilgiri.math.autodiff.Variable;

public class TaylorSeriesTest {
	// scuffed copy the variables because they can't be imported
	public static final DoubleRealFactory NUMBER_FACTORY = TaylorSeries.NUMBER_FACTORY;
	public static final DifferentialRealFunctionFactory<DoubleReal> DIFFERENTIAL_FUNCTION_FACTORY = TaylorSeries.DIFFERENTIAL_FUNCTION_FACTORY;
	public static final Variable<DoubleReal> X = TaylorSeries.X;

	private static final double TOLERANCE = 1e-5;

	public static void main(String[] args) {
		int numRuns;
		if (args.length >= 1) numRuns = Integer.parseInt(args[0]);
		else numRuns = 1;

		System.out.printf("Running everything %d times just for fun\n\n", numRuns);
		for (int i=0; i<numRuns; i++) {
			// Example: compute the Taylor series of sin(x) at x = 0
			DifferentialFunction<DoubleReal> sin = DIFFERENTIAL_FUNCTION_FACTORY.sin(X);
			testAll(sin, 0, 5, new double[] { 0, Math.PI / 2, Math.PI, 3 * Math.PI / 2, 2 * Math.PI });
			testLast(sin, 0, 50, new double[] { 0, Math.PI / 2, Math.PI, 3 * Math.PI / 2, 2 * Math.PI });

			DifferentialFunction<DoubleReal> log = DIFFERENTIAL_FUNCTION_FACTORY.log(X);
			// taylor series for log(x) from a=1 is only stable for x in (0, 1)
			testLast(log, 1, 100_000, new double[] { Math.pow(Math.E, -10), 0.1, 0.5, 0.9, Math.pow(Math.E, -0.1) });

			DifferentialFunction<DoubleReal> exp = DIFFERENTIAL_FUNCTION_FACTORY.exp(X);
			// taylor series for e^x is only stable for x in [0, 1)
			testLast(exp, 1, 2_000, new double[] { 0, 0.0001, 0.1, 0.5, 0.999 });

			DifferentialFunction<DoubleReal> invOneMinusX = DIFFERENTIAL_FUNCTION_FACTORY.val(NUMBER_FACTORY.one()).minus(X).pow(-1);
			// taylor series for 1/(1-x) is only stable for x in (-1, 1) i think
			testLast(invOneMinusX, 0, 2_000, new double[] { -0.9, -0.5, -0.1, 0, 0.1, 0.5, 0.9 });

			DifferentialFunction<DoubleReal> hyperbolicSine = DIFFERENTIAL_FUNCTION_FACTORY.exp(X).minus(DIFFERENTIAL_FUNCTION_FACTORY.exp(X.negate())).div(DIFFERENTIAL_FUNCTION_FACTORY.val(NUMBER_FACTORY.val(2)));
			// taylor series for sinh(x) is only stable for x in (-1, 1) i think
			testLast(hyperbolicSine, 0, 2_000, new double[] { -0.9, -0.5, -0.1, 0, 0.1, 0.5, 0.9 });

			DifferentialFunction<DoubleReal> sinc = DIFFERENTIAL_FUNCTION_FACTORY.sin(X).div(X);
			// taylor series for sinc(x) is always stable ?
			testLast(sinc, 0.0001, 20, new double[] { -Math.PI, -0.5, -0.1, 0.0001, 0.1, 0.5, Math.PI });

			DifferentialFunction<DoubleReal> expDivCos = DIFFERENTIAL_FUNCTION_FACTORY.exp(X).div(DIFFERENTIAL_FUNCTION_FACTORY.cos(X));
			// taylor series for e^x/cos(x)
			testLast(expDivCos, 0.001, 10, new double[] { -1, -0.9, -0.5, -0.1, 0, 0.1, 0.5, 0.9, 1 });

			DifferentialFunction<DoubleReal> expOfSin = DIFFERENTIAL_FUNCTION_FACTORY.exp(DIFFERENTIAL_FUNCTION_FACTORY.sin(X));
			// taylor series for e^(sin(x)) is always stable ?
			testLast(expOfSin, 0, 10, new double[] { -2, -Math.PI / 2, -0.1, 0, 0.1, Math.PI / 2, 2 });

			DifferentialFunction<DoubleReal> sinSquaredXSquaredTimesExp = DIFFERENTIAL_FUNCTION_FACTORY.sin(X.pow(2)).pow(2).mul(DIFFERENTIAL_FUNCTION_FACTORY.exp(X));
			// probably only stable between (0, 1)
			testLast(sinSquaredXSquaredTimesExp, 1, 10, new double[] { 0.1, 0.2, 0.5, 0.9, 0.99 });
		}

		System.out.println("Done!");
	}

	/** tests all approximations with degrees 0 to the target */
	public static boolean testAll(DifferentialFunction<DoubleReal> func, double basis, int degree, double[] pointsToEvaluate) {
		TaylorSeries taylor = new TaylorSeries(func, basis);
		while (taylor.approximationDegree() < degree) {
			taylor.resolve();
			System.out.println("Degree " + taylor.approximationDegree() + " approximation of " + func + " from " + basis + ": ");
			for (double point : pointsToEvaluate) {
				System.out.printf("(x = %f) Actual: %f, Approx: %f\n", point, taylor.actualValue(point), taylor.approximatedValue(point));
			}
			System.out.println();
		}

		for (double point : pointsToEvaluate) {
			double expected = taylor.actualValue(point);
			double actual = taylor.approximatedValue(point);

			if (Math.abs(expected - actual) > TOLERANCE) {
				return false;
			}
		}
		return true;
	}

	/** only tests the highest degree */
	public static boolean testLast(DifferentialFunction<DoubleReal> func, double basis, int degree, double[] pointsToEvaluate) {
		TaylorSeries taylor = new TaylorSeries(func, basis);
		taylor.resolveToAtLeast(degree);
		System.out.println("Degree " + taylor.approximationDegree() + " approximation of " + func + " from " + basis + ": ");
		boolean result = true;
		for (double point : pointsToEvaluate) {
			System.out.printf("(x = %f) Actual: %f, Approx: %f\n", point, taylor.actualValue(point), taylor.approximatedValue(point));
			double expected = taylor.actualValue(point);
			double actual = taylor.approximatedValue(point);

			result &= Math.abs(expected - actual) > TOLERANCE;
		}
		System.out.println();

		return result;
	}
}
