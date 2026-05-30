      *================================================================*
      * DESCRICAO  : ENQUADRAMENTO DA TAXA DA OPERACAO                 *
      * COPYBOOK   : DCOMWAOE - BOOK DE ACESSO AO PROGRAMA FUNCIONAL   *
      * COORDENADOR: DCOM1AOC - OBTER ENQUADRAMENTO DA TAXA            *
      * FLUXO      : DCOMIABE                                          *
      * DATA       : 31/01/2024                                        *
      * AUTOR      : LUCIANDRA SILVEIRA - WIPRO                        *
      * COMPONENTE : DCOM - DESCONTO COMERCIAL                         *
      *----------------------------------------------------------------*
      ************************* BLOCO DE ENTRADA ***********************
      **                                                              **
      * DCOMWAOE-FUNCAO   =   IDENTIFICA OPCAO PARA PROCESSAMENTO      *
      *-> OBRIGATORIO       # 01 --> OBTEM ENQUADRAMENTO DE TAXAS      *
      *                          --> EFETUA  CALCULOS DE JURO  PARA OS *
      *                              TITULOS SELECIONADOS CONSIDERANDO *
      *                              A TAXA  MAXIMA ENQUADRADA  PARA A *
      *                              OPERACAO.                         *
      *                          --> EFETUA CALCULOS DO DEMONSTRATIVO  *
      *                              FINANCEIRO DA PROPOSTA CONSIDERAN-*
      *                              DO A TAXA MAXIMA ENQUADRADA.      *
      *                     # 02 --> EFETUA CALCULOS DE JUROS PARA OS  *
      *                              TITULOS SELECIONADOS CONSIDERAN-  *
      *                              DO A TAXA INFORMADA.              *
      **                                                              **
      *                     # 03 --> EFETUA ENQUADRAMENTO PARA BANDAS  *
      *                              DE TAXAS DA FICO                  *
      *                          --> EFETUA  CALCULOS DE JUROS PARA OS *
      *                              TITULOS SELECIONADOS CONSIDERANDO *
      *                              A TAXA MAXIMA INFORMADA PELO FICO *
      **                                                              **
      *================================================================*
      *
          05 DCOMWAOE-E-HEADER.
             10 DCOMWAOE-COD-LAYOUT    PIC X(008) VALUE 'DCOMWAOE'.
             10 DCOMWAOE-TAM-LAYOUT    PIC 9(005) VALUE 00500.
      *
          05 DCOMWAOE-BLOCO-ENTRADA.
             10 DCOMWAOE-FUNCAO                   PIC  9(002).
             10 DCOMWAOE-CCLUB                    PIC  9(010).
             10 DCOMWAOE-CCNPJ-CPF                PIC  9(009).
             10 DCOMWAOE-CFLIAL-CNPJ              PIC  9(004).
             10 DCOMWAOE-CCTRL-CNPJ-CPF           PIC  9(002).
             10 DCOMWAOE-CPRODT-SERVC-OPER        PIC  9(008).
             10 DCOMWAOE-NSMULA-OPER-DESC         PIC  9(009).
             10 DCOMWAOE-CPPSTA-PRODT             PIC  9(019).
      *
      *-->   PARAMETROS DO SUBPRODUTO DCOMB068 - TSPROD-REGRA-OPER
             10 DCOMWAOE-CINDCD-CALC-JURO         PIC  X(001).
             10 DCOMWAOE-CINDCD-INCID-IOF         PIC  X(001).
      *
      *-->   OPCAO DO CLIENTE PARA O CALCULO DE IOF
      *==>      REDUZ IOF? "S" - REDUZIDA/ "N" - NORMAL
             10 DCOMWAOE-FLAG-RED-IOF             PIC  X(001).
      *==>      ADICIONA IOF?  "S" - SIM / "N" - NAO
             10 DCOMWAOE-FLAG-ADIC-IOF            PIC  X(001).
      *==>      CALCULA CUSTO? "S" - SIM / "N" - NAO
             10 DCOMWAOE-FLAG-CALC-CUSTO          PIC  X(001).
      *
      *-->   DADOS PARA FUNCAO 001
      *-->   -> CALCULAR DEMONSTRATIVO PARA TAXA MAXIMA
      *-->   =================================================
      *
             10 DCOMWAOE-CSGMTO-CLI               PIC  9(003).
             10 DCOMWAOE-CRTING                   PIC  X(003).
             10 DCOMWAOE-NNVEL-RTING              PIC  9(002).
             10 DCOMWAOE-HINIC-VGCIA-NVEL         PIC  X(026).
      *
      *-->   DADOS PARA FUNCAO 002
      *-->   -> CALCULAR DEMONSTRATIVO PARA TAXA INFORMADA
      *-->   =================================================
      *
             10 DCOMWAOE-PTX-JURO                 PIC  9(003)V9(005).
             10 DCOMWAOE-CINDCD-FREQ-TX           PIC  X(002).
      *
      *-->   INTERVALO DE TAXA DO SUBPRODUTO (MELHOR ENQRAD. PROPOSTA)
             10 DCOMWAOE-PMIN-TX-JURO-REGRA       PIC  9(003)V9(003).
             10 DCOMWAOE-PMAX-TX-JURO-REGRA       PIC  9(003)V9(003).
             10 DCOMWAOE-CINDCD-FREQ-TX-REG       PIC  X(002).
      *
      *-->   DADOS DE AUTORIZACAO DA TESOURARIA
             10 DCOMWAOE-CAUTRZ-TX-TESOU          PIC  9(009).
             10 DCOMWAOE-IAUTRZ-TX-TESOU          PIC  X(040).
      *
             10 DCOMWAOE-FILLER                   PIC  X(312).
      *
