      *****************************************************************
      * NOME BOOK : CRURWCAA                                          *
      * DESCRICAO : BOOK INTERFACE - CRUR3CEL                         *
      * DATA      : 28/05/2019                                        *
      * AUTOR     : MILTON JANUARIO                                   *
      * COMPONENTE: CRUR                                              *
      * TAMANHO   : 14276 BYTES                                       *
      *****************************************************************
      ********************  DESCRICAO DOS CAMPOS **********************
      *                                                               *
      * BASE-CALCULO             : 360 DIAS OU 365 DIAS               *
      * QTD-OCORRENCIAS          : QUANTIDADE DE OCORRENCIA ENTRADA   *
      * SEG-FINANC               : INDICADOR DE SEGURO FINANCIADO     *
      * TXA-JUR-ANO              : TAXA DE JUROS ANO                  *
      *                                                               *
      * DATA                     : DATA DO EVENTO                     *
      * TP-EVENTO                : TIPO DE EVENTO                     *
      *                            L - LIBERACAO                      *
      *                            I - IOF                            *
      *                            S - SEGURO                         *
      * VALOR                    : VALOR DO EVENTO                    *
      **************************  RETORNOS  ***************************
      * CET-DIA                  : TAXA INTERNA DE RETORNO AO DIA     *
      * CET-MES                  : TAXA INTERNA DE RETORNO AO MES     *
      * CET-ANO                  : TAXA INTERNA DE RETORNO AO ANO     *
      *****************************************************************
       05 CRURWCAA-HEADER.
           10 CRURWCAA-COD-LAYOUT       PIC X(008)   VALUE 'CRURWCAA'.
           10 CRURWCAA-TAM-LAYOUT       PIC 9(005)   VALUE 14276.
       05 CRURWCAA-REGISTRO.
           10 CRURWCAA-BLOCO-ENTRADA.
            15 CRURWCAA-E-BASE-CALCULO     PIC  9(03).
            15 CRURWCAA-E-TXA-JUR-ANO      PIC  9(05)V9999.
            15 CRURWCAA-E-QTD-OCORRENCIAS  PIC  9(03).
            15 CRURWCAA-E-OCORRENCIAS   OCCURS 200 TIMES.
              20 CRURWCAA-E-DATA           PIC  X(10).
              20 CRURWCAA-E-TP-EVENTO      PIC  X(01).
              20 CRURWCAA-E-VALOR          PIC S9(15)V9(02).
           10 CRURWCAA-S-BLOCO-SAIDA.
            15 CRURWCAA-S-CET-DIA          PIC S9(08)V9(07).
            15 CRURWCAA-S-CET-MES          PIC S9(08)V9(07).
            15 CRURWCAA-S-QTD-OCORRENCIAS  PIC  9(03).
            15 CRURWCAA-S-OCORRENCIAS   OCCURS 200 TIMES.
              20 CRURWCAA-S-DATA           PIC  X(10).
              20 CRURWCAA-S-TP-EVENTO      PIC  X(01).
              20 CRURWCAA-S-VALOR          PIC S9(15)V9(02).
              20 CRURWCAA-S-CET-LIB        PIC S9(08)V9(07).
