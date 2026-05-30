      *===============================================================*
      * INCLUDE PARA TABELA:                                          *
      *    - DCOMB0PA - DB2PRD.TCOMPS_REJTD_DESC                      *
      *---------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                          *
      *    - DCOM7PAC - LISTA                                         *
      *===============================================================*
       01  AJ-ARGUMENTOS-ENTRADA.
      *---------------------------------------------------------------*
      * CASO A CONEXAO DB2 UTILIZADA SEJA CKRS DEVERA INFORMAR
      * PARCIAL-TI  CASO CONTRARIO SERA UTILIZADO BRAD
      *---------------------------------------------------------------*
       05  AJ-INSTRUCAO                PIC  X(010).
      *---------------------------------------------------------------*
      *  INFORMAR A PAGINA QUE DESEJA SER EXIBIDA
      *---------------------------------------------------------------*
       05  AJ-E-NCONS-REJTD-DESC       PIC S9(005) COMP-3.

      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  AJ-RETORNO.
       05  AJ-S-COD-RETORNO            PIC  9(004).
       05  AJ-S-MSG-RETORNO            PIC  X(079).

      *****************************************************************
      * AREA DA LISTA                                                 *
      *****************************************************************
       01  AJ-ARGUMENTOS-SAIDA.
       05  AJ-S-QTD-REG               PIC S9(02) COMP-3.
       05  AJ-S-LISTA                 OCCURS 100 TIMES.
           10 AJ-S-DANO-OPER-DESC     PIC S9(04) COMP-3.
           10 AJ-S-NSEQ-OPER-DESC     PIC S9(09) COMP-3.
           10 AJ-S-HINCL-REG          PIC  X(26).
