      *********************************************************
      * SISTEMA   : DCOM - DESCONTO COMERCIAL  NOME: I#DCOMBA *
      * CRIACAO   : 05/2022                                   *
      * DESCRICAO : ALTERACAO DE OPERACAO                     *
      *                                                       *
      * APLICACAO : MODULO DCOM5594                           *
      *                                                       *
      * TAMANHO   : 116 BYTES                                 *
      *                                                       *
      *********************************************************
      * OBS: ALTERA DADOS BASICOS DA OPERACAO PARA            *
      *      TOMAR OU SENSIBILIZAR CONE NA  SIMULACAO         *
      *      NETEMPRESA - ORPAG                               *
      *                                                       *
      *********************************************************
      * AREA DE ENTRADA                                       *
      *********************************************************
       01  DCOMBA-ENTRADA.
           03 DCOMBA-COD-RETORNO         PIC  9(004).
           03 DCOMBA-MSG-RETORNO         PIC  X(075).
           03 DCOMBA-RESTART             PIC  9(005).
           03 DCOMBA-FLAG                PIC  X(001).
           03 DCOMBA-CFUNC-BDSCO         PIC  9(009).
           03 DCOMBA-CTERM               PIC  X(008).
           03 DCOMBA-DADOS.
               05 DCOMBA-NUM-SIMULACAO   PIC  9(009).
               05 DCOMBA-COD-FUNCAO      PIC  X(001).
               05 DCOMBA-DANO-OPER-DESC  PIC  9(004).
               05 DCOMBA-NSEQ-OPER-DESC  PIC  9(009).
      *- CHAVE DO CONE
               05 DCOMBA-DANO-BASE       PIC  9(004).
               05 DCOMBA-CDEPDC          PIC  9(005).
               05 DCOMBA-CSEQ-STUDO      PIC  9(009).
               05 DCOMBA-CCART-LIM       PIC  X(005).
