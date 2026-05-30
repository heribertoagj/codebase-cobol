      *===============================================================*
      * INCLUDE PARA TABELA:                                          *
      *    - DCOMB0PC - DB2PRD.TOPER_REJTD_DESC                       *
      *---------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                          *
      *    - DCOM7PBI - INSERT                                        *
      *===============================================================*
       01  AF-ARGUMENTOS-ENTRADA.
      *---------------------------------------------------------------*
      * CASO A CONEXAO DB2 UTILIZADA SEJA CKRS DEVERA INFORMAR
      * PARCIAL-TI  CASO CONTRARIO SERA UTILIZADO BRAD
      *---------------------------------------------------------------*
       05  AF-INSTRUCAO                PIC  X(010).

       05  AF-COLUNAS-DA-TABELA.
           10 AF-E-NCONS-REJTD-DESC     PIC S9(05) COMP-3.
           10 AF-E-NOPER-REJTD-DESC     PIC S9(07) COMP-3.
           10 AF-E-CREFT-OPER-RURAL     PIC X(11).
           10 AF-E-NORD-DSTNO-ORIGN     PIC S9(03) COMP-3.
           10 AF-E-DANO-REFT-REJTD      PIC S9(06) COMP-3.
           10 AF-E-CSIT-OPER-RURAL      PIC X(02).
           10 AF-E-CMOTVO-RCUSA-DESC    PIC X(03).
           10 AF-E-HIMPOR-ARQ-CREDT     PIC X(26).
           10 AF-E-HINCL-REG            PIC X(26).

      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  AF-RETORNO.
       05  AF-S-COD-RETORNO              PIC  9(004).
       05  AF-S-MSG-RETORNO              PIC  X(079).


