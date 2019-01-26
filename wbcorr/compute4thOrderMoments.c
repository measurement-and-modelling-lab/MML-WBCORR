void compute4thOrderMoments(double* moments, double* Z, int* p, int* n) {
    int a = 0;
    for (int i = 0; i < *p; i++) {
    	for (int j = 0; j <= i; j++) {
    	    for (int k = 0; k <= j; k++) {
    		for (int h = 0; h <= k; h++) {
    		    for (int b = 0; b < *n; b++) {
    			moments[a] = moments[a] +
    			    *(Z + (*n * i + b)) *
    			    *(Z + (*n * j + b)) *
    			    *(Z + (*n * k + b)) *
    			    *(Z + (*n * h + b));
    		    }
		    a++;
    		}
    	    }
    	}
    }
}
