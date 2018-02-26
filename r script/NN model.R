# Import data in h2oFrame converting into h2o frame

s_file<-as.h2o(df_alldata)

class(s_file)

# Check the data in file

dim(s_file)

head(s_file,1)

# Show all data objects on the H2O platform

h2o.ls()

NN_model = h2o.deeplearning(
  x = 1:21,                # Last column is label
  training_frame = s_file,
  hidden = c(10, 5, 4, 5, 10 ),    # Autoencoder layers
  epochs = 600,
  activation = "Tanh",
  autoencoder = TRUE
)


#Extract the non-linear feature from layer 3 of H2O data set
#using an H2O deep learning model
df_all_supervised_features2 = h2o.deepfeatures(NN_model, s_file, layer=3)


head(df_all_supervised_features2) # Note column names, Layer3.Col1, Layer3.Col2


#Make a dataframe of 2+1 columns
df = as.data.frame(df_all_supervised_features2)  # 2 columns
df$label = as.character(as.vector(s_file[,22])) # +1 column
dim(df)


