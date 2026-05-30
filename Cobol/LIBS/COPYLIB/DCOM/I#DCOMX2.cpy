      *********************************************************
      * SISTEMA   : DCOM  DESCONTO COMERCIAL   NOME: I#DCOMX2 *
      * CRIACAO   : 18/07/2006                                *
      * DESCRICAO : AREA PARA CONSULTA DE DESCRICOES DE       *
      *             ELEMENTOS DO SISTEMA                      *
      * APLICACAO : MODULO - DCOM5079                         *
      *                                                       *
      * TAMANHO   : ENTRADA = LRECL - 0000                    *
      *             SAIDA   = LRECL - 0000                    *
      *********************************************************
       01  X2-ENTRADA-ROTEADOR.
           03  X2-ENT-COD-RETORNO                PIC  X(004).
           03  X2-ENT-MSG-RETORNO                PIC  X(079).
           03  X2-ENT-RESTART                    PIC  9(005).
           03  X2-ENT-FLAG                       PIC  X(001).
           03  X2-ENT-CFUNC-BDSCO                PIC  9(009).
           03  X2-ENT-CTERM                      PIC  X(008).
           03  X2-ENT-CPRODT                     PIC  9(003).
           03  X2-ENT-CSPROD                     PIC  9(003).
           03  X2-ENT-CMEIO-ENTRD                PIC  9(003).
           03  X2-ENT-CMEIO-LIBR                 PIC  9(003).
           03  X2-ENT-CMEIO-PGTO                 PIC  9(003).
           03  X2-ENT-PRODUTO                    PIC  X(004).
           03  X2-ENT-OPERACAO                   PIC  9(007).
       01  X2-SAIDA-ROTEADOR.
           03  X2-SAI-HEADER.
               05  X2-SAI-COD-RETORNO            PIC X(004).
               05  X2-SAI-MSG-RETORNO            PIC X(079).
               05  X2-SAI-RESTART                PIC 9(005).
               05  X2-SAI-FLAG                   PIC X(001).
           03  X2-SAI-DADOS.
               05  X2-SAI-CPRODT                 PIC 9(003).
               05  X2-SAI-IPRODT                 PIC X(040).
               05  X2-SAI-CSPROD                 PIC 9(003).
               05  X2-SAI-ISPROD                 PIC X(040).
               05  X2-SAI-IRSUMO-SPROD           PIC X(010).
               05  X2-SAI-CSGL-SPROD             PIC X(006).
               05  X2-SAI-CTPO-DESC-COML         PIC 9(003).
               05  X2-SAI-IRSUMO-TPO-DESC        PIC X(010).
               05  X2-SAI-CSIT-DESC-COML         PIC 9(003).
               05  X2-SAI-HSIT-DESC-COML         PIC X(022).
               05  X2-SAI-IRSUMO-SIT-DESC        PIC X(015).
               05  X2-SAI-CMEIO-ENTRD            PIC 9(003).
               05  X2-SAI-IMEIO-ENTRD            PIC X(040).
               05  X2-SAI-CMEIO-LIBR             PIC 9(003).
               05  X2-SAI-IMEIO-LIBR             PIC X(040).
               05  X2-SAI-CMEIO-PGTO             PIC 9(003).
               05  X2-SAI-IMEIO-PGTO             PIC X(040).
               05  X2-SAI-CFUNC-BDSCO-ALT        PIC 9(009).
               05  X2-SAI-NFUNC-BDSCO-ALT        PIC X(040).
               05  X2-SAI-CFUNC-BDSCO-INC        PIC 9(009).
               05  X2-SAI-NFUNC-BDSCO-INC        PIC X(040).
               05  X2-SAI-HULT-ATULZ-ALT         PIC X(022).
               05  X2-SAI-HULT-ATULZ-INC         PIC X(022).
               05  X2-SAI-HULT-ATULZ             PIC X(022).
               05  X2-SAI-CFUNC-BDSCO            PIC 9(009).
               05  X2-SAI-NFUNC-BDSCO            PIC X(040).
               05  X2-SAI-RESPOSTA-CTBIL         PIC 9(003).
               05  X2-SAI-STATUS-CTBIL           PIC X(001).
               05  X2-SAI-COPER-CTBIL            PIC X(007).
               05  X2-SAI-CSEQ-TRILA-CTBIL       PIC 9(004).
               05  X2-SAI-RTRILA-CTBIL           PIC X(030).
               05  X2-SAI-DINIC-VGCIA-CTBIL      PIC X(010).
               05  X2-SAI-DFIM-VGCIA-CTBIL       PIC X(010).
               05  X2-SAI-CIDTFD-LCTO-CTBIL      PIC 9(005).
