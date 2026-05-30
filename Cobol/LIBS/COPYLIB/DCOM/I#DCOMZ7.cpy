      *********************************************************
      * SISTEMA   : DCOM - DESCONTO COMERCIAL  NOME: I#DCOMZ7 *
      * CRIACAO   : 11/2006                                   *
      * DESCRICAO : AREA PARA CHAMADA DO PROGRAMA DCOM5371    *
      *             (SENSIBILIZA CONVENIO)                    *
      *                                                       *
      * APLICACAO : MODULO DCOM5320 (INCLUSAO DOS DADOS       *
      *             BASICOS DA OPERACAO)                      *
      *                                                       *
      * TAMANHO   :                                           *
      *                                                       *
      *********************************************************

      *********************************************************
      * AREA DE ENTRADA                                       *
      *********************************************************

       01  ROTENT-5371.
              03     ROTENT-5371-COD-RETORNO        PIC  X(04).
              03     ROTENT-5371-MSG-RETORNO        PIC  X(79).
              03     ROTENT-5371-RESTART            PIC  9(05).
              03     ROTENT-5371-FLAG               PIC  X(01).
              03     ROTENT-5371-CFUNC-BDSCO        PIC  9(09).
              03     ROTENT-5371-CTERM              PIC  X(08).
              03     ROTENT-5371-CCONVE-DESC        PIC  9(09).
              03     ROTENT-5371-CTPO-CONVE-DESC    PIC  9(01).
              03     ROTENT-5371-CIND-SOMA-SUBTRAI  PIC  9(01).
      *                                1-SOMA E 2-SUBTRAI
              03     ROTENT-5371-VDIFER-CONVE-DESC  PIC  9(15)V99.

      *********************************************************
      * AREA DE SAIDA                                         *
      *********************************************************

       01  ROTSAI-5371.
              03     ROTSAI-5371-HEADER.
                05   ROTSAI-5371-COD-RETORNO        PIC  X(04).
                05   ROTSAI-5371-MSG-RETORNO        PIC  X(79).
                05   ROTSAI-5371-RESTART            PIC  9(05).
                05   ROTSAI-5371-FLAG               PIC  X(01).
                05   ROTSAI-5371-COUNT              PIC  9(09).

              03     ROTSAI-5371-CONSISTENCIA.
                05   ROTSAI-5371-TAMANHO-ERRO       PIC  9(01).
                05   ROTSAI-5371-TABELA-ERROS       OCCURS 04  TIMES.
                  07 ROTSAI-5371-CAMPOS-ERRO        PIC  9(01).

