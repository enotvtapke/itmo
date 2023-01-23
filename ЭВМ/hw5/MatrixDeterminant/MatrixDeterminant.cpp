#include <iostream>
#include <random>
#include <cmath>
#include <ctime>
#include <omp.h>
#include <fstream>

using namespace std;

int LUPDecompose(double** a, int n, double tol);
int LUPDecomposeParallel(double** a, int n, double tol);
double LUPDeterminant(double** a, int p, int n);
void fillMatrix(double** a, int n, FILE* in);
void fillMatrixRandom(double** a, int n, int abs = 10);
double fRand(double fMin, double fMax);
void printMatrix(double** a, int n);
double** copyMatrix(double** a, int n);
void DeleteMatrix(double** a, int n);
double* testParallel(double** matrix, int n, int threads);
double* test(double** matrix, int n);
double averageTime(double** a, int n, int nthreads, int ntests, bool parallel = true);

#define SHEDULE dynamic
#define TOL 0.0001

int main(int argc, char* argv[]) {
    if (argc < 3 || argc > 4) {
        printf("Invalid number of arguments");
        return 1;
    }
    char* p;
    int nthreads = strtol(argv[1], &p, 10);
    if (*p || nthreads < -1) {
        printf("Invalid number of threads");
        return 1;
    }
    
    FILE* in;
    fopen_s(&in, argv[2], "r");
    if (!in) {
        printf("Can't open file: %s", argv[2]);
        return 1;
    }
    if (feof(in)) {
        printf("Unexpected end of file");
        return 1;
    }
    int n = 0;
    fscanf_s(in, "%i", &n);
    
    if (n <= 0) {
        printf("Invalid matrix size");
        return 1;
    }
    double** matrix = new double* [n];
    for (int i = 0; i < n; i++) {
        matrix[i] = new double[n];
    }
    fillMatrix(matrix, n, in);
    fclose(in);
    FILE* out;
    if (argc == 4) {
        fopen_s(&out, argv[3], "w");
        if (!out) {
            printf("Can't open file: %s", argv[3]);
            return 1;
        }
    } else {
        out = stdout;
    }
    double* result;
    if (nthreads == -1) {
        result = test(matrix, n);
        fprintf(out, "Determinant: %g\n", result[1]);
        printf("\nTime (without OpenMP): %f ms\n", result[0]);
    } else {
        result = testParallel(matrix, n, nthreads);
        fprintf(out, "Determinant: %g\n", result[1]);
        printf("\nTime (%i thread(s)): %f ms\n", nthreads, result[0]);
    }
    fclose(out);
    DeleteMatrix(matrix, n);
}

double averageTime(double** a, int n, int nthreads, int ntests, bool parallel) {
    double* res = new double[ntests];
    for (int i = 0; i < ntests; i++) {
        res[i] = parallel ? testParallel(a, n, nthreads)[0] : test(a, n)[0];
    }
    double sum = 0;
    for (int i = 0; i < ntests; i++) {
        sum += res[i];
    }
    return sum / (double) ntests;
}

void fillMatrix(double** a, int n, FILE* in) {
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            if (feof(in)) {
                printf("Unexpected end of file");
                exit(1);
            }
            fscanf_s(in, "%lg", &a[i][j]);
        }
    }
}

double* testParallel(double** matrix, int n, int nthreads) {
    double** a = copyMatrix(matrix, n);
    if (nthreads != 0) {
        omp_set_num_threads(nthreads);
    }
    double start_time = omp_get_wtime();
    int p = LUPDecomposeParallel(a, n, TOL);
    double det = LUPDeterminant(a, p, n);
    double end_time = omp_get_wtime();
    DeleteMatrix(a, n);
    return new double[] {(end_time - start_time) * 1000, det};
}

double* test(double** matrix, int n) {
    double** a = copyMatrix(matrix, n);
    double start_time = omp_get_wtime();
    int p = LUPDecompose(a, n, TOL);
    double det = LUPDeterminant(a, p, n);
    double end_time = omp_get_wtime();
    DeleteMatrix(a, n);
    return new double[] {(end_time - start_time) * 1000, det};
}

void DeleteMatrix(double** a, int n) {
    for (int i = 0; i < n; i++) {
        delete[] a[i];
    }
    delete[] a;
}

double** copyMatrix(double** a, int n) {
    double** b = new double* [n];
    for (int i = 0; i < n; i++) {
        b[i] = new double[n];
        for (int j = 0; j < n; j++) {
            b[i][j] = a[i][j];
        }
    }
    return b;
}

void printMatrix(double** a, int n) {
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            printf("%5.3g\t", a[i][j]);
        }
        printf("\n");
    }
    printf("\n");
}

void fillMatrixRandom(double** a, int n, int abs) {
    srand(time(0));
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            a[i][j] = fRand(-abs, abs);
        }
    }
    a[0][0] = 0;
    if (n > 1) {
        a[1][1] = 0;
    }
}

double fRand(double fMin, double fMax) {
    double f = (double)rand() / RAND_MAX;
    return fMin + f * (fMax - fMin);
}

int LUPDecompose(double** a, int n, double tol) {
    int i, j, k, imax;
    double maxA, * ptr, absA;
    int p = 0;

    for (i = 0; i < n; i++) {
        maxA = fabs(a[i][i]);
        imax = i;
        if (maxA < tol) {
            for (k = i; k < n; k++) {
                if ((absA = fabs(a[k][i])) > maxA) {
                    maxA = absA;
                    imax = k;
                }
            }

            if (maxA < tol) return -1; 
            
            ptr = a[i];
            a[i] = a[imax];
            a[imax] = ptr;
            p++;
        }
        for (j = i + 1; j < n; j++) {
            a[j][i] /= a[i][i];
            for (k = i + 1; k < n; k++) {
                a[j][k] -= a[j][i] * a[i][k];
            }
        }
    }
    return p;
}

int LUPDecomposeParallel(double** a, int n, double tol) {
    int i, j, k, imax;
    double maxA, * ptr, absA;
    int p = 0;

    for (i = 0; i < n; i++) {
        maxA = fabs(a[i][i]);
        imax = i;
        if (maxA < tol) {
            for (k = i; k < n; k++) {
                if ((absA = fabs(a[k][i])) > maxA) {
                    maxA = absA;
                    imax = k;
                }
            }

            if (maxA < tol) return -1;

            ptr = a[i];
            a[i] = a[imax];
            a[imax] = ptr;
            p++;
        }
        #pragma omp parallel for schedule(SHEDULE)
        for (j = i + 1; j < n; j++) {
            a[j][i] /= a[i][i];
            for (int p = i + 1; p < n; p++) {
                a[j][p] -= a[j][i] * a[i][p];
            }
        }
    }
    return p;
}

double LUPDeterminant(double** a, int p, int n) {
    if (p == -1) {
        return 0;
    }
    double det = a[0][0];
    for (int i = 1; i < n; i++) {
        det *= a[i][i];
    }
    return p % 2 == 0 ? det : -det;
}