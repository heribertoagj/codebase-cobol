      *===============================================================*
      * INCLUDE PARA TABELA:                                          *
      *    - DCOMB0PB - DB2PRD.TCONS_REJTD_DESC                       *
      *---------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                          *
      *    - DCOM7PBI - INSERT                                        *
      *===============================================================*
       01  AE-ARGUMENTOS-ENTRADA.
      * CASO A CONEXAO DB2 UTILIZADA SEJA CKRS DEVERA INFORMAR
      * PARCIAL-TI  CASO CONTRARIO SERA UTILIZADO BRAD
       05  AE-INSTRUCAO                PIC  X(010).

       05  AE-COLUNAS-DA-TABELA.
           10 AE-NCONS-REJTD-DESC     PIC S9(05) COMP-3.
           10 AE-CCNPJ-ENTID-DESC     PIC S9(09) COMP-3.
           10 AE-CFLIAL-CNPJ-DESC     PIC S9(04) COMP-3.
           10 AE-CCTRL-CNPJ-DESC      PIC S9(02) COMP-3.
           10 AE-DANO-INIC-DESC       PIC S9(06) COMP-3.
           10 AE-DANO-FIM-DESC        PIC S9(06) COMP-3.
           10 AE-CREFT-OPER-RURAL     PIC X(11).
           10 AE-QCDULA-CREDT-DESC    PIC S9(07) COMP-3.
           10 AE-HINCL-REG            PIC X(26).

      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  AE-RETORNO.
       05  AE-COD-RETORNO              PIC  9(004).
       05  AE-MENSAGEM                 PIC  X(079).


