import nilgiri.math.*;
import nilgiri.math.autodiff.*;

import java.math.BigInteger;
import java.math.RoundingMode;

/**
 * A class to compute the Taylor series approximation of a given function with respect to X.
 * This class is not thread-safe.
 */
public class TaylorSeries {
    public static final DoubleRealFactory NUMBER_FACTORY = DoubleRealFactory.instance();
    public static final DifferentialRealFunctionFactory<DoubleReal> DIFFERENTIAL_FUNCTION_FACTORY = new DifferentialRealFunctionFactory<>(NUMBER_FACTORY);
    public static final Variable<DoubleReal> X = DIFFERENTIAL_FUNCTION_FACTORY.var("x", NUMBER_FACTORY.zero());

    private final DifferentialFunction<DoubleReal> originalFunction;
    private final DoubleReal basis; // a
    private DifferentialFunction<DoubleReal> approximation;
    private DifferentialFunction<DoubleReal> lastDerivative;
    private double factorial;
    private int approximationDegree;

    public TaylorSeries(DifferentialFunction<DoubleReal> originalFunction, DoubleReal basis) {
        this.originalFunction = originalFunction;
        this.basis = basis;
        approximation = null;
        lastDerivative = null;
        approximationDegree = -1;
        factorial = 0;
    }

    public TaylorSeries(DifferentialFunction<DoubleReal> originalFunction, double basis) {
        this(originalFunction, NUMBER_FACTORY.val(basis));
    }

    public TaylorSeries(DifferentialFunction<DoubleReal> originalFunction) {
        this(originalFunction, NUMBER_FACTORY.zero());
    }

    /** resolves another degree of approximation */
    public void resolve() {
        approximationDegree++;
        X.set(basis);
        if (approximationDegree == 0) {
            approximation = DIFFERENTIAL_FUNCTION_FACTORY.val(originalFunction.getValue());
            lastDerivative = originalFunction;
            factorial = 1;
        } else {
            lastDerivative = lastDerivative.diff(X);
            DoubleReal derivativeValue = lastDerivative.getValue();

            factorial *= approximationDegree;

            if (derivativeValue.doubleValue() == 0) return;
            if (Double.isNaN(derivativeValue.doubleValue() / factorial)) return;

            var base = basis.doubleValue() != 0 ? X.minus(DIFFERENTIAL_FUNCTION_FACTORY.val(basis)) : X; // simplify expression tree

            var term = base.pow(approximationDegree)
                    .mul(DIFFERENTIAL_FUNCTION_FACTORY.val(derivativeValue))
                    .div(DIFFERENTIAL_FUNCTION_FACTORY.val(NUMBER_FACTORY.val(factorial)));

            if (lastDerivative.isConstant() && derivativeValue.doubleValue() == 0) return;

            approximation = approximation.plus(term);
        }
    }

    public void resolveToAtLeast(int degree) {
        while (approximationDegree < degree) {
            resolve();
        }
    }

    public double actualValue(double x) {
        X.set(NUMBER_FACTORY.val(x));
        return originalFunction.getValue().doubleValue();
    }

    public double approximatedValue(double x) {
        X.set(NUMBER_FACTORY.val(x));
        return approximation.getValue().doubleValue();
    }

    public int approximationDegree() {
        return approximationDegree;
    }

    public DifferentialFunction<DoubleReal> approximationFunction() {
        return approximation;
    }
}