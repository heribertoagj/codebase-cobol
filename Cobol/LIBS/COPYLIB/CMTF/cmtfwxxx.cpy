      ******************************************************************
      * NOME BOOK : GOTFW99C                                           *
      * DESCRICAO : AREA DE COMUNICACAO - DATAS DE PROCESSAMENTOS      *
      * DATA      : FEVEREIRO/2012                                     *
      * AUTOR     : SONDA PROCWORK                                     *
      * EMPRESA   : SONDA PROCWORK                                     *
      * GRUPO     : GOTF                                               *
      * TAMANHO   : 0047                                               *
      ******************************************************************
      *                                                                *
      * GOTFW99C-CSIST-PRINC           = CODIGO DO SISTEMA             *
      * GOTFW99C-DPROCM-ANTER          = DATA DO PROCESSAMENTO ANTERIOR*
      * GOTFW99C-DPROCM-ATUAL          = DATA DO PROCESSAMENTO ATUAL   *
      * GOTFW99C-DPROCM-PROX           = DATA DO PROXIMO PROCESSAMENTO *
      * GOTFW99C-DPROCM-ESPEL          = DATA DE ESPELHAMENTO          *
      *                                                                *
      ******************************************************************
        05 GOTFW99C-REGISTRO.
           10 GOTFW99C-CSIST-PRINC          PIC X(04).
           10 GOTFW99C-DPROCM-ANTER         PIC X(10).
           10 GOTFW99C-DPROCM-ATUAL         PIC X(10).
           10 GOTFW99C-DPROCM-PROX          PIC X(10).
           10 GOTFW99C-DPROCM-ESPEL         PIC X(10).
           10 FILLER                        PIC X(03).
