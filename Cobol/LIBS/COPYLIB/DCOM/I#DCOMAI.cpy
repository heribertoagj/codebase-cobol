      *===============================================================*
      * INCLUDE PARA TABELA:                                          *
      *    - DCOMB0PB - DB2PRD.TCONS_REJTD_DESC                       *
      *---------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                          *
      *    - DCOM7PBC - CONSULTA                                      *
      *===============================================================*
       01  AI-ARGUMENTOS-ENTRADA.
      *---------------------------------------------------------------*
      * CASO A CONEXAO DB2 UTILIZADA SEJA CKRS DEVERA INFORMAR
      * PARCIAL-TI  CASO CONTRARIO SERA UTILIZADO BRAD
      *---------------------------------------------------------------*
       05  AI-INSTRUCAO                PIC  X(010).

       05  AI-E-CCNPJ-ENTID-DESC       PIC S9(09) COMP-3.
       05  AI-E-CFLIAL-CNPJ-DESC       PIC S9(04) COMP-3.
       05  AI-E-CCTRL-CNPJ-DESC        PIC S9(02) COMP-3.
       05  AI-E-DANO-INIC-DESC         PIC S9(06) COMP-3.
       05  AI-E-DANO-FIM-DESC          PIC S9(06) COMP-3.
       05  AI-E-NUM-REF-OCOR           PIC  X(11).
      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  AI-RETORNO.
       05  AI-S-COD-RETORNO            PIC  9(004).
       05  AI-S-MSG-RETORNO            PIC  X(079).

      *****************************************************************
      * AREA DA LISTA                                                 *
      *****************************************************************
       01  AI-ARGUMENTOS-SAIDA.
           10 AI-S-NCONS-REJTD-DESC   PIC S9(05) COMP-3.
           10 AI-S-CREFT-OPER-RURAL   PIC  X(11).
           10 AI-S-QCDULA-CREDT-DESC  PIC S9(07) COMP-3.
           10 AI-S-HINCL-REG          PIC X(26).
