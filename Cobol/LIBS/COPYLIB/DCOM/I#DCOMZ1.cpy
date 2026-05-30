      *********************************************************
      * SISTEMA   : DCOM - DESCONTO COMERCIAL  NOME: I#DCOMZ1 *
      * CRIACAO   : 11/2006                                   *
      * DESCRICAO : AREA PARA CHAMADA DO PROGRAMA DCOM5450    *
      *             (INCLUSAO DE PESSOA)                      *
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

       01 ENTRADA-5450-ROTEADOR.
          03 ROTENT-5450-HEADER.
             05 ROTENT-5450-COD-RETORNO            PIC  X(04).
             05 ROTENT-5450-MSG-RETORNO            PIC  X(79).
             05 ROTENT-5450-RESTART                PIC  9(05).
             05 ROTENT-5450-FLAG                   PIC  9(01).
             05 ROTENT-5450-START-COUNT            PIC  X(01).
             05 ROTENT-5450-CFUNC-BDSCO            PIC  9(09).
             05 ROTENT-5450-CTERM                  PIC  X(08).

          03 ROTENT-5450-DADOS.
      * DADOS DE TPSSOA_DESC_COML
             05 ROTENT-5450-CCNPJ-CPF              PIC 9(09).
             05 ROTENT-5450-CFLIAL-CNPJ            PIC 9(05).
             05 ROTENT-5450-CCTRL-CNPJ-CPF         PIC 9(02).
             05 ROTENT-5450-CDOCTO-ID              PIC X(15).
             05 ROTENT-5450-IPSSOA-DESC-COML       PIC X(70).
             05 ROTENT-5450-CCLUB                  PIC 9(10).

      * DADOS DE TRELAC_PSSOA_DESC
             05 ROTENT-5450-CINDCD-PSSOA-DESC      PIC X(02).
             05 ROTENT-5450-CELMTO-DESC-COML       PIC 9(03).
             05 ROTENT-5450-CCHAVE-ELMTO-DESC      PIC X(45).
             05 ROTENT-5450-CBCO                   PIC 9(03).
             05 ROTENT-5450-CAG-BCRIA              PIC 9(05).
             05 ROTENT-5450-CCTA-BCRIA-CLI         PIC 9(13).
             05 ROTENT-5450-CPOSTO-SERVC           PIC 9(03).
             05 ROTENT-5450-IPSSOA-CONJG           PIC X(40).
             05 ROTENT-5450-CCPF-CONJG             PIC 9(09).
             05 ROTENT-5450-CCTRL-CPF-CONJG        PIC 9(02).
             05 ROTENT-5450-CDOCTO-ID-CONJG        PIC X(15).
             05 ROTENT-5450-CINDCD-CONJG-AUTRZ     PIC X(01).
             05 ROTENT-5450-CCEP                   PIC 9(05).
             05 ROTENT-5450-CCEP-COMPL             PIC 9(03).
             05 ROTENT-5450-ELOGDR                 PIC X(70).
             05 ROTENT-5450-NUMERO-LOGRADOURO      PIC X(07).
             05 ROTENT-5450-COMPL-LOGRADOURO       PIC X(30).
             05 ROTENT-5450-IBAIRO                 PIC X(40).
             05 ROTENT-5450-IMUN                   PIC X(50).
             05 ROTENT-5450-CSGL-UF                PIC X(02).

      *********************************************************
      * AREA DE SAIDA                                         *
      *********************************************************

       01 SAIDA-5450-ROTEADOR.
          03 ROTSAI-5450-HEADER.
             05 ROTSAI-5450-COD-RETORNO           PIC X(04).
             05 ROTSAI-5450-MSG-RETORNO           PIC X(79).
             05 ROTSAI-5450-RESTART               PIC 9(05).
             05 ROTSAI-5450-FLAG                  PIC 9(01).
             05 ROTSAI-5450-QTDE-RETORNADA        PIC 9(03).
             05 ROTSAI-5450-COUNT                 PIC 9(09).

          03 ROTSAI-5450-DADOS.
             05 ROTSAI-5450-NPSSOA-DESC-COML      PIC 9(09).
             05 ROTSAI-5450-NRELAC-PSSOA-DESC     PIC 9(09).
      *
      *   03 ROTSAI-5450-CONSISTENCIA REDEFINES ROTSAI-5450-DADOS.
      *      05 ROTSAI-5450-TAMANHO-ERRO          PIC  9(01).
      *      05 ROTSAI-5450-TABELA-ERROS   OCCURS   22   TIMES.
      *         07 ROTSAI-5450-CAMPOS-ERRO        PIC  9(01).
