      *================================================================*
      * DESCRICAO  : ENQUADRAMENTO DA TAXA DA OPERACAO                 *
      * COPYBOOK   : DCOMWAOS - BOOK DE ACESSO AO PROGRAMA FUNCIONAL   *
      * COORDENADOR: DCOM1AOC - OBTER ENQUADRAMENTO DA TAXA            *
      * FLUXO      : DDCOMIABE                                         *
      * DATA       : 05/02/2024                                        *
      * AUTOR      : LUCIANDRA SILVEIRA - WIPRO                        *
      * COMPONENTE : DCOM - DESCONTO COMERCIAL                         *
      *----------------------------------------------------------------*
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      *----------------------------------------------------------------*
      * XX/XX/XXXX XXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  ***
      *================================================================*
      *
          05 DCOMWAOS-HEADER.
             10 DCOMWAOS-COD-LAYOUT    PIC X(008)   VALUE 'DCOMWAOS'.
             10 DCOMWAOS-TAM-LAYOUT    PIC 9(005)   VALUE 00500.
      *
          05 DCOMWAOS-BLOCO-SAIDA.
             10 DCOMWAOS-TOPER-DESC-APURA           PIC  9(003).
             10 DCOMWAOS-VOPER-APURA-SINAL          PIC  X(001).
             10 DCOMWAOS-VOPER-APURA-DESC           PIC  9(015)V9(002).
             10 DCOMWAOS-QTITLO-OPER-APURA          PIC  9(009).
             10 DCOMWAOS-DVCTO-FNAL-APURA           PIC  X(010).
             10 DCOMWAOS-TMED-PONDE-OPER            PIC  9(005).
             10 DCOMWAOS-CTPO-TX-JURO-DESC          PIC  9(003).
             10 DCOMWAOS-ITPO-TX-JURO-DESC          PIC  X(030).
             10 DCOMWAOS-PALIQT-IOF-DESC            PIC  9(003)V9(005).
             10 DCOMWAOS-PTX-JURO-DIA               PIC  9(003)V9(005).
             10 DCOMWAOS-PTX-JURO-MES               PIC  9(003)V9(005).
             10 DCOMWAOS-PTX-JURO-ANO               PIC  9(003)V9(005).
             10 DCOMWAOS-VJURO-OPER-SINAL           PIC  X(001).
             10 DCOMWAOS-VJURO-OPER-DESC            PIC  9(015)V9(002).
             10 DCOMWAOS-VTARIF-SINAL               PIC  X(001).
             10 DCOMWAOS-VTARIF-REG-TITLO           PIC  9(015)V9(002).
             10 DCOMWAOS-VTAC-SINAL                 PIC  X(001).
             10 DCOMWAOS-VTAC-OPER-DESC             PIC  9(015)V9(002).
             10 DCOMWAOS-VIOF-SINAL                 PIC  X(001).
             10 DCOMWAOS-VIOF-OPER-DESC             PIC  9(015)V9(002).
             10 DCOMWAOS-VLIQ-SINAL                 PIC  X(001).
             10 DCOMWAOS-VLIQ-OPER-DESC             PIC  9(015)V9(002).
             10 DCOMWAOS-QTITLO-CHEQ-PCELA          PIC  9(009).
             10 DCOMWAOS-PALIQT-IOF-REDZD           PIC  9(003)V9(005).
             10 DCOMWAOS-CINDCD-ALIQT-REDZD         PIC  X(001).
      *
      *-->   SPREAD E CUSTO RETORNADOS PELO COTX
             10 DCOMWAOS-PTX-CUSTO-AA               PIC  9(007)V9(005).
             10 DCOMWAOS-PTX-CUSTO-AM               PIC  9(007)V9(005).
             10 DCOMWAOS-PSPREAD-PR-AA-SINAL        PIC  X(001).
             10 DCOMWAOS-PSPREAD-PRECO-AA           PIC  9(007)V9(005).
             10 DCOMWAOS-PSPREAD-PR-AM-SINAL        PIC  X(001).
             10 DCOMWAOS-PSPREAD-PRECO-AM           PIC  9(007)V9(005).
      *
      *-->   INTERVALO DE TAXA DO PRODUTO (SEGURANCA)
             10 DCOMWAOS-PMIN-TX-JURO-PRODT         PIC  9(003)V9(003).
             10 DCOMWAOS-PMAX-TX-JURO-PRODT         PIC  9(003)V9(003).
             10 DCOMWAOS-CINDCD-FREQ-TX-PR          PIC  X(002).
      *
      *-->   INTERVALO DE TAXA DO SUBPRODUTO (MELHOR ENQUAD. PROPOSTA)
             10 DCOMWAOS-PMIN-TX-JURO-REGRA         PIC  9(003)V9(003).
             10 DCOMWAOS-PMAX-TX-JURO-REGRA         PIC  9(003)V9(003).
             10 DCOMWAOS-CINDCD-FREQ-TX             PIC  X(002).
      *
      *-->   TAXA MINIMA ESPECIAL DO CLIENTE
             10 DCOMWAOS-PTX-JURO-ESP-CLI           PIC  9(003)V9(003).
             10 DCOMWAOS-CINDCD-FREQ-TX-CLIE        PIC  X(002).
      *
      *-->   ENQUADROU TAXA A PARTIR DO SPREAD DA AUTORIZACAO TESOURARIA
      *-->   TAXA DIFERENTE DA INFORMADA? ("S" - SIM / "N" - N�O)
             10 DCOMWAOS-FLAG-TX-SPREAD             PIC  X(001).
      *
      *-->   ENQUADRAMENTO GEROU SPREAD NEGATIVO, NECESSARIO AUTORIZACAO
      *-->   DA TESOURARIA? ("S" - SIM / "N" - N�O)
             10 DCOMWAOS-FLAG-TESOURARIA            PIC  X(001).
      *
      *-->   AREA RESERVADA PARA EXPANS�O FUTURA
             10 FILLER                              PIC  X(181).
