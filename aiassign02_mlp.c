#include<stdio.h>
#include<stdlib.h>
#include<math.h>
#include<time.h>
#include <limits.h>
typedef struct neuron
{	
	double *weight;
	double value;
	double error;
}NEURON;
double sigmoid(double x){

    return 1.0/(1.0 + exp(0.0-x));
}

//Derivative of Sigmoid function
double sigmoidPrime(double x){

    return sigmoid(x)*(1-sigmoid(x));
}
NEURON** buildperceptron(int l[],int n){
	NEURON** network=(NEURON **)malloc(n*sizeof(NEURON*));
    int i,j,m;
    for(i=0;i<n;i++)
    {

        network[i]=(NEURON *)malloc((l[i]+1)*sizeof(NEURON));
    }
    for(i=1;i<n;i++)
    {
        for(j=0;j<=l[i];j++)
        {
            if(j==0)
            {
                network[i][j].value=1.0;
                network[i][j].weight=NULL;
                
            }
            else
            {
                network[i][j].value=0.0;
                network[i][j].weight=(double *)malloc((l[i-1]+1)*sizeof(double));
                for(m=0;m<=l[i-1];m++)
                {
                    network[i][j].weight[m]=0.1+(rand()/9999999999.0);
                }
            }
            network[i][j].error=0.0;
        }
    }
    return network;
}
void Inputinit(NEURON* perceptron[],double input[],int numNeuLayer[],int numOfLayers)
{
	int j,i=0;
	for(j=0;j<=numNeuLayer[i];j++)
	{
		if(j==0)
            {
                perceptron[i][j].value=1.0;
                
            }
            else
            {
            	perceptron[i][j].value=input[j-1];
                
            }
            perceptron[i][j].weight=NULL;
                perceptron[i][j].error=0.0;
	}
}

void initialize(double input[],int target[],double* ARR[],int c,int i)
{
	int j;
		for(j=0;j<10;j++)
		{
			target[j]=0;
		}
		for (j = 0; j < c; j++)
		{
			if(j==0)
			{
				target[(int)ARR[i][0]-1]=1;
			}
			else
			{
				input[j-1]=ARR[i][j];
			}
		}
}
void Activation(NEURON* perceptron[],int layers[],int l)
{
	int i,j,k;
	for(i=1;i<l;i++)
	{
		for(j=1;j<=layers[i];j++)
		{
			for(k=0;k<=layers[i-1];k++)
			{
				if(i==1 || k==0)
				{
					perceptron[i][j].value+=perceptron[i][j].weight[k]*perceptron[i-1][j].value;
				}
				else
				{
					perceptron[i][j].value+=perceptron[i][j].weight[k]*sigmoid(perceptron[i-1][k].value);
				}
				}
			}
		}
	}

void Errorbackprop(NEURON* perceptron[],int layers[],int l,int target[])
{
	int i,j,k;
	
	for(i=l-1;i>0;i--)
	{
		for(j=1;j<=layers[i];j++)
		{
            if(i==l-1)
            {
                perceptron[i][j].error=(target[j-1]-sigmoid(perceptron[i][j].value))*sigmoidPrime(perceptron[i][j].value);

            }
            else
            {
                for(k=1;k<=layers[i+1];k++){

                    perceptron[i][j].error+=(perceptron[i+1][k].error*perceptron[i+1][k].weight[j]*sigmoidPrime(perceptron[i][j].value));
                }
            }
        }
	}
}
void Weight(NEURON* perceptron[],int layers[],int l,float ta)
{
	int i,j,k;
    for(i=1;i<l;i++){

        for(j=1;j<=layers[i];j++){
        	
            for(k=0;k<=layers[i-1];k++){

                if(i==1 || k==0){
                	//
                    perceptron[i][j].weight[k]+=ta*perceptron[i-1][k].value*perceptron[i][j].error;
                    
                }
                else{
                    perceptron[i][j].weight[k]+=ta*sigmoid(perceptron[i-1][k].value)*perceptron[i][j].error;
                }
            }
        }
    }
}
void NeuralUpdate(NEURON* perceptron[],int layers[],int l,int target[],float ta)
{
	Activation(perceptron,layers,l);
	Errorbackprop(perceptron,layers,l,target);
	Weight(perceptron,layers,l,ta);
}
double** ReadFromFile(char file[],int rows,int columns){

	double** arr = (double **)malloc((rows-1)*sizeof(double*));
	for(int i=0;i<rows;i++){
		arr[i] = (double *)malloc(columns*sizeof(double));
	}
	FILE* fp = fopen(file,"r");
	int iv=0,i=0,n=0,j=0;
	char v[12],ch;
	while((ch=getc(fp))!=EOF){
		if(iv==0){
			if(ch=='\n'){
				iv=1;
			}
		}
		else{
			if(ch==',' || ch=='\n'){
				v[n]='\0';
				arr[i][j]=atoi(v);
				j++;
				j=j%columns;
				if(j==0)
				{
					i++;
					i=i%rows;
				}
				n=0;
			}
			else{
				v[n]=ch;
				n++;
				n=n%12;
			}
		}
	}
	return arr;

}

double** normalize(double** train, int rows, int columns){
	int i,j,k,min,max;
	for(i=1;i<=columns;i++){
		min = INT_MAX;
		max = INT_MIN;
		for(j=1;j<=rows-1;j++){
				if(train[j][i] < min ){
					min = train[j][i];
				}
				if(train[j][i] > max){
					max = train[j][i];
				}
		}
		for(k=1;k<=rows-1;k++){
			train[k][i] = (train[k][i]-min)/(max-min);
		}
	}
	return train;
}

int main(){

int loop,arr1[10],index=0;
double arr2[10];
for(loop=5;loop<=10;loop++)
{
	int numNeuLayer[] = {16,loop,10};
	int numOfLayers = 3;
	int epoches,i,j;
	double alpha = 0.15;

	srand((unsigned int) time(NULL));
	double **train,**test; 
    char trainf[]="C:/Users/maahi/Desktop/trainData.csv";
    int trainFileRows=2217,trainFileCols=17;
    char testf[]="C:/Users/maahi/Desktop/testData.csv";
    int testFileRows=999,testFileCols=17;
    
    NEURON** perceptron = buildperceptron(numNeuLayer,numOfLayers);
    
   	train = ReadFromFile(trainf,trainFileRows,trainFileCols);
    test = ReadFromFile(testf,testFileRows,testFileCols);

    train=normalize(train,trainFileRows,trainFileCols);
    test=normalize(test,testFileRows,testFileCols);

    /*for(i=1;i<trainFileRows-1;i++){
    	for (j = 1; j < trainFileCols-1;j++)
    	{
    		printf("%lf,",train[i][j] );
    	}
    	printf("\n");
    }*/

	int* target=(int *)malloc(10*sizeof(int));
	double* input=(double *)malloc((trainFileCols-1)*sizeof(double));
				
	for (i = 0; i < trainFileRows-1; i++)
	{
		epoches=0;
		while(epoches<100)
		{
		initialize(input,target,train,trainFileCols,i);
		Inputinit(perceptron,input,numNeuLayer,numOfLayers);
		NeuralUpdate(perceptron,numNeuLayer,numOfLayers,target,alpha);
		epoches++;	
		}
		
	}

	double accuracy;
    int correct=0,incorrect=0;
    int maxClass;
    for(i=0;i<testFileRows-1;i++){
        for(j=1;j<testFileCols;j++){

            input[j-1]=test[i][j];
        }

        
        Inputinit(perceptron,input,numNeuLayer,numOfLayers);
		NeuralUpdate(perceptron,numNeuLayer,numOfLayers,target,alpha);
        double maxAct=-1.0;
        for(j=1;j<=numNeuLayer[numOfLayers-1];j++){

            if(sigmoid(perceptron[numOfLayers-1][j].value) > maxAct){

                maxAct=sigmoid(perceptron[numOfLayers-1][j].value);
                maxClass=j;
            }
        }
        //printf("Sigmoid:%lf\n",sigmoid(perceptron[numOfLayers-1][maxClass].value));
        if(maxClass==((int)test[i][0])){

            correct+=1;
        }
        else{

            incorrect+=1;
        }
    }
    printf("correct:%d\n",correct);
    accuracy=(correct*1.0/(correct+incorrect))*100.0; //Network Accuracy On Testing Set.

    //printNeuralNetwork(network,layers,nl); //Print Neural Network
   printf("ACCURACY for %d hidden nodes: %lf\n",loop,accuracy);
   arr1[index] = loop;
   arr2[index] = accuracy;
   index++;
   free(perceptron);
  }
  char * commandsForGnuplot[] = {"set title \"TITLEEEEE\"", "plot 'data.temp' with linespoints title 'accuracy'", "set xlabel 'no of nodes in hidden layer'" , "set ylabel 'accuracy'"};
   // double xvals[NUM_POINTS] = {1.0, 2.0, 3.0, 4.0, 5.0};
   // double yvals[NUM_POINTS] = {5.0 ,3.0, 1.0, 3.0, 5.0};
    FILE * temp = fopen("data.temp", "w");
    FILE * gnuplotPipe = popen ("gnuplot -persistent", "w");
    int i;
    for (i=0; i < 6 ; i++)
    {
    fprintf(temp, "%d %lf \n", arr1[i], arr2[i]); //Write the data to a temporary file
    }

    for (i=0; i < 4 ; i++)
    {
    fprintf(gnuplotPipe, "%s \n", commandsForGnuplot[i]); //Send commands to gnuplot one by one.
    }
return 0;
}