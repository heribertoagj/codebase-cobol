      *********************************************************
      * SISTEMA   : DCOM - DESCONTO COMERCIAL  NOME: I#DCOMXM *
      * CRIACAO   : 08/2007                                   *
      * DESCRICAO : PESQUISA RATING - LISTA E INDIVIDUAL      *
      * APLICACAO : MODULO DCOM6446                           *
      * TAMANHO   :                                           *
      *********************************************************
      * AREA DE ENTRADA                                       *
      *********************************************************
       01  ENTRADA-6446-ROTEADOR.
           03  ROTENT-6446-COD-RETORNO        PIC  X(004).
           03  ROTENT-6446-MSG-RETORNO        PIC  X(079).
           03  ROTENT-6446-RESTART            PIC  9(005).
           03  ROTENT-6446-FLAG               PIC  X(001).
           03  ROTENT-6446-START-COUNT        PIC  X(001).
           03  ROTENT-6446-CFUNC-BDSCO        PIC  9(009).
           03  ROTENT-6446-CTERM              PIC  X(008).
           03  ROTENT-6446-TP-PROC            PIC  X(001).
     ***** (L) = LISTA *** (U) = UNICO *** (V) = VIGENTE
           03  ROTENT-6446-CD-RATING          PIC  X(003).

       01  SAIDA-6446-ROTEADOR.
           03  ROTSAI-6446-HEADER.
               05  ROTSAI-6446-COD-RETORNO    PIC  X(004).
               05  ROTSAI-6446-MSG-RETORNO    PIC  X(079).
               05  ROTSAI-6446-RESTART        PIC  9(005).
               05  ROTSAI-6446-FLAG           PIC  X(001).
               05  ROTSAI-6446-QTDE-RETORNADA PIC  9(003).
               05  ROTSAI-6446-COUNT          PIC  9(009).

           03  ROTSAI-6446-DADOS.
               05  ROTSAI-6446-TABELA-SAIDA OCCURS  100  TIMES.
                   07  ROTSAI-6446-CRTING     PIC  X(03).
                   07  ROTSAI-6446-ICRTING    PIC  X(015).

           03  ROTSAI-6446-CONSISTENCIA       REDEFINES
               ROTSAI-6446-DADOS.
               05  ROTSAI-6446-TAMANHO-ERRO   PIC  9(001).
               05  ROTSAI-6446-TABELA-ERROS OCCURS  002  TIMES.
                   07 ROTSAI-6446-CAMPOS-ERRO PIC  9(001).
