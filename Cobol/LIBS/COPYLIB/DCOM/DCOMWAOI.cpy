      *================================================================*
      * DESCRICAO  : ENQUADRAMENTO DA TAXA DA OPERACAO                 *
      * COPYBOOK   : DCOMWAOI - BOOK DE ACESSO AO PROGRAMA FUNCIONAL   *
      * COORDENADOR: DCOM1AOC - OBTER ENQUADRAMENTO DA TAXA            *
      * FUNCIONAL  : DCOM3AOC - OBTER ENQUADRAMENTO DA TAXA            *
      * FLUXO      : DCOMIABE                                          *
      * DATA       : 25/03/2024                                        *
      * AUTOR      : LUCIANDRA SILVEIRA - WIPRO                        *
      * COMPONENTE : DCOM - DESCONTO COMERCIAL                         *
      *----------------------------------------------------------------*
      ************************* BLOCO DE ENTRADA ***********************
      **                                                              **
      * DCOMWAOI-E-FUNCAO =   IDENTIFICA OPCAO PARA PROCESSAMENTO      *
      *-> OBRIGATORIO       # 01 --> OBTEM ENQUADRAMENTO DE TAXAS      *
      *                          --> EFETUA  CALCULOS DE JURO  PARA OS *
      *                              TITULOS SELECIONADOS CONSIDERANDO *
      *                              A TAXA  MAXIMA ENQUADRADA  PARA A *
      *                              OPERACAO.                         *
      *                          --> EFETUA CALCULOS DO DEMONSTRATIVO  *
      *                              FINANCEIRO DA OPERACAO.           *
      *                     # 02 --> EFETUA CALCULOS DE JUROS PARA OS  *
      *                              TITULOS SELECIONADOS CONSIDERAN-  *
      *                              DO A TAXA INFORMADA.              *
      ************************* BLOCO DE ENTRADA ***********************
          05 DCOMWAOI-HEADER.
             10 DCOMWAOI-COD-LAYOUT  PIC X(008)      VALUE 'DCOMWAOI'.
             10 DCOMWAOI-TAM-LAYOUT  PIC 9(005)      VALUE 01400.
      *
      *-->   BLOCO DE ENTRADA
      *-->   ========================
          05 DCOMWAOI-REGISTRO.
      *-->   DADOS DA SESSAO DE LOGIN DO USUARIO
             10 DCOMWAOI-BLOCO-SESSAO.
                15 DCOMWAOI-CCANAL                PIC  9(003).
                15 DCOMWAOI-CAUTEN-SEGRC.
                   20 DCOMWAOI-CAUTEN-SEGRC-N     PIC  9(009).
                15 DCOMWAOI-DEPENDENCIA-OPER      PIC  9(005).
                15 DCOMWAOI-EMPR-USUAR-TRAB       PIC  9(010).
                15 DCOMWAOI-DEPEND-USUAR-TRAB     PIC  9(008).
                15 DCOMWAOI-CODIGO-DEPENDENCIA    PIC  9(005).
                15 DCOMWAOI-CTERM                 PIC  X(008).
      *
      *-->   DADOS DE ENTRADA
             10 DCOMWAOI-BLOCO-ENTRADA.
                15 DCOMWAOI-E-FUNCAO              PIC  9(002).
                15 DCOMWAOI-E-CCLUB               PIC  9(010).
                15 DCOMWAOI-E-CCNPJ-CPF           PIC  9(009).
                15 DCOMWAOI-E-CFLIAL-CNPJ         PIC  9(004).
                15 DCOMWAOI-E-CCTRL-CNPJ-CPF      PIC  9(002).
                15 DCOMWAOI-E-CPRODT-SERVC-OPER   PIC  9(008).
                15 DCOMWAOI-E-NSMULA-OPER-DESC    PIC  9(009).
                15 DCOMWAOI-E-CPPSTA-PRODT        PIC  9(019).
      *
      *-->   PARAMETROS DO SUBPRODUTO DCOMB068 - TSPROD-REGRA-OPER
                15 DCOMWAOI-E-CINDCD-CALC-JURO    PIC  X(001).
                15 DCOMWAOI-E-CINDCD-INCID-IOF    PIC  X(001).
      *
      *-->   OPCAO DO CLIENTE PARA O CALCULO DE IOF E CUSTO
                15 DCOMWAOI-E-FLAG-RED-IOF        PIC  X(001).
      *==>      REDUZ IOF? "S" - REDUZIDA/ "N" - NORMAL
                15 DCOMWAOI-E-FLAG-ADIC-IOF       PIC  X(001).
      *==>      ADICIONA IOF?  "S" - SIM / "N" - NAO
                15 DCOMWAOI-E-FLAG-CALC-CUSTO     PIC  X(001).
      *==>      CALCULA CUSTO? "S" - SIM / "N" - NAO
      *
      *-->   DADOS PARA FUNCAO 001
      *-->   -> CALCULAR DEMONSTRATIVO PARA TAXA MAXIMA
      *-->   =================================================
      *
                15 DCOMWAOI-E-CSGMTO-CLI          PIC  9(003).
                15 DCOMWAOI-E-CRTING              PIC  X(003).
                15 DCOMWAOI-E-NNVEL-RTING         PIC  9(002).
                15 DCOMWAOI-E-HINIC-VGCIA-NVEL    PIC  X(026).
      *
      *-->   DADOS PARA FUNCAO 002
      *-->   -> CALCULAR DEMONSTRATIVO PARA TAXA INFORMADA
      *-->   =================================================
      *
                15 DCOMWAOI-E-PTX-JURO            PIC  9(003)V9(005).
                15 DCOMWAOI-E-CINDCD-FREQ-TX      PIC  X(002).
      *
      *-->   INTERVALO DE TAXA DO SUBPRODUTO (MELHOR ENQRAD. PROPOSTA)
                15 DCOMWAOI-E-PMIN-TX-JURO-REGRA  PIC  9(003)V9(003).
                15 DCOMWAOI-E-PMAX-TX-JURO-REGRA  PIC  9(003)V9(003).
                15 DCOMWAOI-E-CINDCD-FREQ-TX-REG  PIC  X(002).
      *
      *-->   DADOS DE AUTORIZACAO DA TESOURARIA
                15 DCOMWAOI-E-CAUTRZ-TX-TESOU     PIC  9(009).
                15 DCOMWAOI-E-IAUTRZ-TX-TESOU     PIC  X(040).
      *
      *-->   AREA RESERVADA PARA EXPANS�O FUTURA
                15 DCOMWAOI-E-FILLER              PIC  X(225).
      *
      *-->   BLOCO DE SAIDA
      *-->   ========================
             10 DCOMWAOI-BLOCO-SAIDA.
                15 DCOMWAOI-S-TOPER-DESC-APURA    PIC  9(003).
                15 DCOMWAOI-S-VOPER-APURA-SINAL   PIC  X(001).
                15 DCOMWAOI-S-VOPER-APURA-DESC    PIC  9(015)V9(002).
                15 DCOMWAOI-S-QTITLO-OPER-APURA   PIC  9(009).
                15 DCOMWAOI-S-DVCTO-FNAL-APURA    PIC  X(010).
                15 DCOMWAOI-S-TMED-PONDE-OPER     PIC  9(005).
                15 DCOMWAOI-S-CTPO-TX-JURO-DESC   PIC  9(003).
                15 DCOMWAOI-S-ITPO-TX-JURO-DESC   PIC  X(030).
                15 DCOMWAOI-S-PALIQT-IOF-DESC     PIC  9(003)V9(005).
                15 DCOMWAOI-S-PTX-JURO-DIA        PIC  9(003)V9(005).
                15 DCOMWAOI-S-PTX-JURO-MES        PIC  9(003)V9(005).
                15 DCOMWAOI-S-PTX-JURO-ANO        PIC  9(003)V9(005).
                15 DCOMWAOI-S-VJURO-OPER-SINAL    PIC  X(001).
                15 DCOMWAOI-S-VJURO-OPER-DESC     PIC  9(015)V9(002).
                15 DCOMWAOI-S-VTARIF-SINAL        PIC  X(001).
                15 DCOMWAOI-S-VTARIF-REG-TITLO    PIC  9(015)V9(002).
                15 DCOMWAOI-S-VTAC-SINAL          PIC  X(001).
                15 DCOMWAOI-S-VTAC-OPER-DESC      PIC  9(015)V9(002).
                15 DCOMWAOI-S-VIOF-SINAL          PIC  X(001).
                15 DCOMWAOI-S-VIOF-OPER-DESC      PIC  9(015)V9(002).
                15 DCOMWAOI-S-VLIQ-SINAL          PIC  X(001).
                15 DCOMWAOI-S-VLIQ-OPER-DESC      PIC  9(015)V9(002).
                15 DCOMWAOI-S-QTITLO-CHEQ-PCELA   PIC  9(009).
                15 DCOMWAOI-S-PALIQT-IOF-REDZD    PIC  9(003)V9(005).
                15 DCOMWAOI-S-CINDCD-ALIQT-REDZD  PIC  X(001).
      *
      *-->   SPREAD E CUSTO RETORNADOS PELO COTX
                15 DCOMWAOI-S-PTX-CUSTO-AA        PIC  9(007)V9(005).
                15 DCOMWAOI-S-PTX-CUSTO-AM        PIC  9(007)V9(005).
                15 DCOMWAOI-S-PSPREAD-PR-AA-SINAL PIC  X(001).
                15 DCOMWAOI-S-PSPREAD-PRECO-AA    PIC  9(007)V9(005).
                15 DCOMWAOI-S-PSPREAD-PR-AM-SINAL PIC  X(001).
                15 DCOMWAOI-S-PSPREAD-PRECO-AM    PIC  9(007)V9(005).
      *
      *-->   INTERVALO DE TAXA DO PRODUTO (SEGURANCA)
                15 DCOMWAOI-S-PMIN-TX-JURO-PRODT  PIC  9(003)V9(003).
                15 DCOMWAOI-S-PMAX-TX-JURO-PRODT  PIC  9(003)V9(003).
                15 DCOMWAOI-S-CINDCD-FREQ-TX-PR   PIC  X(002).
      *
      *-->   INTERVALO DE TAXA DO SUBPRODUTO (MELHOR ENQUAD. PROPOSTA)
                15 DCOMWAOI-S-PMIN-TX-JURO-REGRA  PIC  9(003)V9(003).
                15 DCOMWAOI-S-PMAX-TX-JURO-REGRA  PIC  9(003)V9(003).
                15 DCOMWAOI-S-CINDCD-FREQ-TX      PIC  X(002).
      *
      *-->   TAXA MINIMA ESPECIAL DO CLIENTE
                15 DCOMWAOI-S-PTX-JURO-ESP-CLI    PIC  9(003)V9(003).
                15 DCOMWAOI-S-CINDCD-FREQ-TX-CLI  PIC  X(002).
      *
      *-->   ENQUADROU TAXA A PARTIR DO SPREAD DA AUTORIZACAO TESOURARIA
      *-->   TAXA DIFERENTE DA INFORMADA? ("S" - SIM / "N" - N�O)
                15 DCOMWAOI-S-FLAG-TX-SPREAD      PIC  X(001).
      *
      *-->   ENQUADRAMENTO GEROU SPREAD NEGATIVO, NECESSARIO AUTORIZACAO
      *-->   DA TESOURARIA? ("S" - SIM / "N" - N�O)
                15 DCOMWAOI-S-FLAG-TESOURARIA     PIC  X(001).
      *
      *-->   AREA RESERVADA PARA EXPANS�O FUTURA
                15 FILLER                         PIC  X(633).
