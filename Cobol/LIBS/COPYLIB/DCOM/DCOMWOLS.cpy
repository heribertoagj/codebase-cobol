      ******************************************************************
      * COPYBOOK  : DCOMWOLS                                           *
      * DESCRICAO : BOOK DE SAIDA - LISTA DE CONTRATOS DCOM            *
      * DATA      : 12/05/2021                                         *
      * AUTOR     : LUIS EDUARDO                                       *
      * EMPRESA   : BRQ                                                *
      * GRUPO     : OPEN BANKING - VS FORNECIMENTO                     *
      * COMPONENTE: DCOM - DESCONTO COMERCIAL                          *
      *----------------------------------------------------------------*
      * ALTERACOES:                                                    *
      *                                                                *
      *    DATA    NOME       DESCRICAO                                *
      * XX/XX/XXXX XXXXXXXXX  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      *                                                                *
      *----------------------------------------------------------------*
      * DCOMWOLS-BLOCO-SAIDA                                           *
      * DCOMWOLS-S-CMODLD-RZ-CTBIL  = TIPO E MODALIDADE RISC           *
      *                                2 PRIMEIRAS TIPO MODALIDADE     *
      *                                2 ULTIMAS MODALIDADE            *
      *  DCOMWOLS-S-DEXPARA-MOD     = DE X PARA DAS MODALIDADES DO RISC*
      *                               PARA O APRESENTADO AO CLIENTE    *
      *  DCOMWOLS-S-WID-PDRAO-OPER  = NUMERO DO CONTRATO IPOC          *
      *  DCOMWOLS-S-SIST-ORIGEM     = SIGLA SISTEMA ORIGEM             *
      *  DCOMWOLS-S-OPERACAO        = ANO/SEQ OPERACAO DESCONTO        *
      *  DCOMWOLS-S-DT-QUITACAO     = DATA QUITACAO DO CONTRATO        *
      *                               'BRACOS' - CONTRATO ATIVO        *
      *  DCOMWOLS-S-CARTEIRA        = CARTEIRA RISC                    *
      ******************************************************************
          05 DCOMWOLS-HEADER.
             10 DCOMWOLS-COD-LAYOUT        PIC X(008) VALUE 'DCOMWOLS'.
             10 DCOMWOLS-TAM-LAYOUT        PIC 9(005) VALUE 13721.
             10 DCOMWOLS-VERSAO            PIC X(005).
          05 DCOMWOLS-BLOCO-SAIDA.
             10 DCOMWOLS-S-QTDE-CONTRATO   PIC  9(003).
             10 DCOMWOLS-S-OCORRENCIAS     OCCURS 100 TIMES.
                15 DCOMWOLS-S-CMODLD-RZ-CTBIL    PIC  9(004).
                15 DCOMWOLS-S-DEXPARA-MOD        PIC  X(036).
                15 DCOMWOLS-S-WID-PDRAO-OPER     PIC  X(067).
                15 DCOMWOLS-S-SIST-ORIGEM        PIC  X(004).
                15 DCOMWOLS-S-OPERACAO           PIC  9(013).
                15 DCOMWOLS-S-DT-QUITACAO        PIC  X(010).
                15 DCOMWOLS-S-CARTEIRA           PIC  X(003).
