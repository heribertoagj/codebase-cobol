      *****************************************************************
      * NOME DA INC - I#GFCTSU                                        *
      * DESCRICAO   - ARQUIVO EVENTOS PARCELADOS CORRENTISTA COMPLETO *
      * TAMANHO     - 100                                             *
      * DATA        - MAI.2006                                        *
      * RESPONSAVEL - RODRIGO PRADO - PROCWORK                        *
      *****************************************************************
      *
        01  GFCTSU-REG-PARCELADOS.
            03 GFCTSU-CHV-CONSOLIDADO.
               10 GFCTSU-IND-TARIF-GRPSERV    PIC 9(001).
      *                   1  = TARIFAS
      *                   2  = GRUPO DE SERVICO
               10 GFCTSU-CD-TARIF-GPSRV       PIC 9(005).
               10 GFCTSU-ANO-MES-REF          PIC 9(006).
               10 GFCTSU-CTPO-CLIE            PIC 9(002).
      *                   01  = CLIENTES CORRENTISTA
      *                   02  = CLIENTES NAO CORRENTISTA
      *                   03  = TODOS
               10 GFCTSU-NUMERO-PARC          PIC 9(005).
               10 GFCTSU-QTD-TOTAL-PARC       PIC 9(005).
            03 GFCTSU-VALOR-POTENCIAL         PIC S9(009)V99.
            03 GFCTSU-VALOR-BRUTO             PIC S9(009)V99.
            03 GFCTSU-VALOR-LIQUIDO           PIC S9(009)V99.
            03 GFCTSU-VALOR-DEBITADO          PIC S9(009)V99.
            03 FILLER                         PIC X(032).
