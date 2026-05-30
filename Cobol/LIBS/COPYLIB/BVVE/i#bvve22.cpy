      *****************************************************************
      *                                                               *
      *   I#BVVE22 - AREA DE VARIAVEIS                                *
      *                                                               *
      *****************************************************************

       01  BVVE22-AREA-VARIAVEIS.
           03  BVVE22-BUFFER-RX.
               05  BVVE22-COD-RETORNO        PIC  9(0002).
               05  BVVE22-SQLCODE            PIC S9(0005).
               05  BVVE22-TAMANHO            PIC  9(0004).
               05  BVVE22-ID-REST            PIC  9(0001).
               05  BVVE22-TAM-REST           PIC  9(0004).
               05  BVVE22-AREA-DADOS         PIC  X(2032).
           03  BVVE22-METADADOS              PIC  X(0500).
           03  BVVE22-TAM-REGISTRO           PIC  9(0004).
           03  BVVE22-REG-ATUAL              PIC  9(0004).
           03  BVVE22-POSICAO-ATUAL          PIC  9(0004).
           03  BVVE22-REG-TOTAL              PIC  9(0004).
