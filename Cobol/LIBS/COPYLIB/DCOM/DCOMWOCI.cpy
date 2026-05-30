      ******************************************************************
      * COPYBOOK  : DCOMWOCI                                           *
      * DESCRICAO : BOOK DE INTERFACE - DETALHE  CONTRATOS CRD. OPBK   *
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
      * DCOMWOCI-BLOCO-ENTRADA                                         *
      * DCOMWOCI-VERSAO             = VERSO DO MODULOS(N OBRIGATORIO)  *
      * DCOMWOCI-E-DSOLTC-CONS-DADOS= DATA DA CONSULTA                 *
      * DCOMWOCI-E-DATA-INICIO      = DATA INICIAL PESQUISA            *
      * DCOMWOCI-E-DATA-FINAL       = DATA FINAL PESQUISA              *
      * DCOMWOCI-E-AGENCIA          = AGENCIA DO CLIENTE               *
      * DCOMWOCI-E-CONTA            = CONTA DO CLIENTE                 *
      * DCOMWOCI-E-CSEQ-TTLAR       = TITULARIDADE DO CLIENTE          *
      * DCOMWOCI-E-CCPF-CNPJ        = PRINCIPAL CPF OU CNPJ            *
      * DCOMWOCI-E-CFLIAL-CNPJ      = FILIAL CPF OU CNPJ               *
      * DCOMWOCI-E-CCTRL-CPF-CNPJ   = CONTROLE CPF OU CNPJ             *
      *----------------------------------------------------------------*
      * DCOMWOCI-BLOCO-SAIDA                                           *
      * DCOMWOCI-S-CMODLD-RZ-CTBIL  = TIPO E MODALIDADE RISC           *
      *                                2 PRIMEIRAS TIPO MODALIDADE     *
      *                                2 ULTIMAS MODALIDADE            *
      *  DCOMWOCI-S-DEXPARA-MOD     = DE X PARA DAS MODALIDADES DO RISC*
      *                               PARA O APRESENTADO AO CLIENTE    *
      *  DCOMWOCI-S-WID-PDRAO-OPER  = NUMERO DO CONTRATO IPOC          *
      *  DCOMWOCI-S-SIST-ORIGEM     = SIGLA SISTEMA ORIGEM             *
      *  DCOMWOCI-S-OPERACAO        = ANO/SEQ OPERACAO DESCONTO        *
      *  DCOMWOLS-S-DT-QUITACAO     = DATA QUITACAO DO CONTRATO        *
      *                               'BRANCOS' - CONTRATO ATIVO       *
      *  DCOMWOCI-S-CARTEIRA        = CARTEIRA RISC                    *
      ******************************************************************
          05 DCOMWOCI-HEADER.
             10 DCOMWOCI-COD-LAYOUT        PIC X(008) VALUE 'DCOMWOCI'.
             10 DCOMWOCI-TAM-LAYOUT        PIC 9(005) VALUE 01073.
             10 DCOMWOCI-VERSAO            PIC X(005).
          05 DCOMWOCI-BLOCO-ENTRADA.
             10 DCOMWOCI-BLOCO-DETCONTR.
                15 DCOMWOCI-E-DSOLTC-CONS-DADOS  PIC  X(010).
                15 DCOMWOCI-E-DATA-INICIO        PIC  X(010).
                15 DCOMWOCI-E-DATA-FIM           PIC  X(010).
                15 DCOMWOCI-E-OPERACAO           PIC  9(013).
             10 FILLER                           PIC  X(100).
          05 DCOMWOCI-BLOCO-SAIDA.
             10 DCOMWOCI-S-OPERACAO              PIC  9(013).
             10 DCOMWOCI-S-WID-PDRAO-OPER        PIC  X(067).
             10 DCOMWOCI-S-DEXPARA-MOD           PIC  X(036).
             10 DCOMWOCI-S-CMODLD-RZ-CTBIL       PIC  9(004).
             10 DCOMWOCI-S-DT-OPERACAO           PIC  X(010).
             10 DCOMWOCI-S-DT-INIC-OPER          PIC  X(010).
             10 DCOMWOCI-S-DT-QUITACAO           PIC  X(010).
             10 DCOMWOCI-S-VL-OPERACAO           PIC  9(015)V9(2).
             10 DCOMWOCI-S-TP-MOEDA              PIC  X(003).
             10 DCOMWOCI-S-DT-FNAL-OPER          PIC  X(010).
             10 DCOMWOCI-S-PERIODICIDADE         PIC  X(025).
             10 DCOMWOCI-S-DT-VCTO-INICIAL       PIC  X(010).
             10 DCOMWOCI-S-VL-CET                PIC  9(008)V9(7).
             10 DCOMWOCI-S-TP-AMORTIZACAO        PIC  X(023).
             10 DCOMWOCI-S-TP-TAXA               PIC  X(007).
             10 DCOMWOCI-S-TP-RATE               PIC  X(008).
             10 DCOMWOCI-S-RATE-FREQ             PIC  X(002).
             10 DCOMWOCI-S-TP-CALC               PIC  9(001).
             10 DCOMWOCI-S-TP-INDEX              PIC  X(010).
             10 DCOMWOCI-S-STP-INDEX             PIC  X(010).
             10 DCOMWOCI-S-INDEX-INFO            PIC  X(005).
             10 DCOMWOCI-S-VL-PRE-RATE           PIC  9(008)V9(7).
             10 DCOMWOCI-S-VL-POS-RATE           PIC  9(008)V9(7).
             10 DCOMWOCI-S-INFO                  PIC  X(005).
             10 DCOMWOCI-S-NM-TARIFA             PIC  X(018).
             10 DCOMWOCI-S-CD-TARIFA             PIC  X(003).
             10 DCOMWOCI-S-TP-TARIFA             PIC  X(005).
             10 DCOMWOCI-S-REGRA-TARIFA          PIC  X(010).
             10 DCOMWOCI-S-VL-TARIFA             PIC  9(015)V9(2).
             10 DCOMWOCI-S-VL-TARIFA-PERC        PIC  9(008)V9(7).
             10 DCOMWOCI-S-QTDE-ENCARGOS         PIC  9(003).
             10 DCOMWOCI-S-OCOR-ENCARGOS   OCCURS 10 TIMES.
                15 DCOMWOCI-S-TP-ENCARGO         PIC  X(035).
                15 DCOMWOCI-S-INFO-ADICIONAL     PIC  X(001).
                15 DCOMWOCI-S-VL-ENGARGO         PIC  9(008)V9(7).
