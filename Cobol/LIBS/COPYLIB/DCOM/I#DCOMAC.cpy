      *===============================================================*
      * INCLUDE PARA TABELA:                                          *
      *    - DCOMB0PA - DB2PRD.TCOMPS_REJTD_DESC                      *
      *---------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                          *
      *    - DCOM7PAI - INSERT                                        *
      *===============================================================*
       01  AC-ARGUMENTOS-ENTRADA.

      *
      * CASO A CONEXAO DB2 UTILIZADA SEJA CKRS DEVERA INFORMAR
      * PARCIAL-TI  CASO CONTRARIO SERA UTILIZADO BRAD
       05  AC-INSTRUCAO                PIC  X(010).

       05  AC-COLUNAS-DA-TABELA.
           10  AC-NCONS-REJTD-DESC     PIC S9(05) COMP-3.
           10  AC-DANO-OPER-DESC       PIC S9(04) COMP-3.
           10  AC-NSEQ-OPER-DESC       PIC S9(09) COMP-3.
           10  AC-HINCL-REG            PIC X(26).
           10  AC-CMSGEM-RECOR         PIC X(09).

      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  AC-RETORNO.
       05  AC-COD-RETORNO              PIC  9(004).
       05  AC-MENSAGEM                 PIC  X(079).


