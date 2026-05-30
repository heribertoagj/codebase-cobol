      *================================================================*
      * DESCRICAO  : CONSULTA / MANUTEN��O DA TESTEMUNHA DE UMA OPERA- *
      *              CAO/PROPOSTA DE ANTECIPACAO - LAYOUT DE ENTRADA   *
      * COPYBOOK   : DCOMWA1I - BOOK INTERMEDIARIO DE ACESSO FUNCIONAL *
      * COORDENADOR: DCOM1ALL - CONTA / MANUTENCAO DE TESTEMUNHAS      *
      * FLUXO      : DCOMIABB                                          *
      * DATA       : 19/06/2024                                        *
      * AUTOR      : LUCIANDRA SILVEIRA - WIPRO                        *
      * COMPONENTE : DCOM - DESCONTO COMERCIAL                         *
      *----------------------------------------------------------------*
      ************************* BLOCO DE ENTRADA ***********************
      **                                                              **
      * DCOMWA1E-FUNCAO       = IDENTIFICA FILTRO DE FUNCIONALIDADE:   *
      *-> OBRIGATORIO         - PARA A AGENCIA INFORMADA:              *
      *->                       01 - LISTAR TESTEMUNHAS                *
      *                         02 - CONSULTAR TESTEMUNHA              *
      *                         03 - INCLUIR TESTEMUNHA                *
      *                         04 - ALTERAR TESTEMUNHA                *
      *                         05 - EXCLUIR TESTEMUNHA                *
      *================================================================*
      *
          05 DCOMWA1I-HEADER.
             10 DCOMWA1I-COD-LAYOUT    PIC X(008) VALUE 'DCOMWA1E'.
             10 DCOMWA1I-TAM-LAYOUT    PIC 9(005) VALUE 13000.
      *
          05 DCOMWA1I-REGISTRO.
             10 DCOMWA1I-BLOCO-ENTRADA.
                15 DCOMWA1I-E-FUNCAO              PIC  9(002).
                15 DCOMWA1I-E-CBCO                PIC  9(003).
                15 DCOMWA1I-E-CAG-BCRIA           PIC  9(005).
                15 DCOMWA1I-E-NPSSOA-DESC-COML    PIC  9(009).
                15 DCOMWA1I-E-CCPF                PIC  9(009).
                15 DCOMWA1I-E-CCTRL-CPF           PIC  9(002).
                15 DCOMWA1I-E-IPSSOA-DESC-COML    PIC  X(060).
                15 DCOMWA1I-E-CCLUB               PIC  9(010).
                15 DCOMWA1I-E-CDOCTO-ID           PIC  X(015).
                15 DCOMWA1I-E-FILLER              PIC  X(172).
      *
             10 DCOMWA1I-BLOCO-SESSAO.
                15 DCOMWA1I-CCANAL                PIC  9(003).
                15 DCOMWA1I-CTERM                 PIC  X(008).
                15 DCOMWA1I-CAUTEN-SEGRC.
                   20 DCOMWA1I-CAUTEN-SEGRC-N     PIC  9(009).
                15 DCOMWA1I-DEPENDENCIA-OPER      PIC  9(005).
                15 DCOMWA1I-EMPR-USUAR-TRAB       PIC  9(010).
                15 DCOMWA1I-DEPEND-USUAR-TRAB     PIC  9(008).
                15 DCOMWA1I-CODIGO-DEPENDENCIA    PIC  9(005).
      *
             10 DCOMWA1I-BLOCO-SAIDA.
                15 DCOMWA1I-QTDE-TESTEMUNHAS      PIC  9(003).
                15 DCOMWA1I-TESTEMUNHAS           OCCURS 100 TIMES.
                   20 DCOMWA1I-S-CBCO             PIC  9(003).
                   20 DCOMWA1I-S-CAG-BCRIA        PIC  9(005).
                   20 DCOMWA1I-S-NPSSOA-DESC-COML PIC  9(009).
                   20 DCOMWA1I-S-CCPF             PIC  9(009).
                   20 DCOMWA1I-S-CCTRL-CPF        PIC  9(002).
                   20 DCOMWA1I-S-IPSSOA-DESC-COML PIC  X(060).
                   20 DCOMWA1I-S-CCLUB            PIC  9(010).
                   20 DCOMWA1I-S-CDOCTO-ID        PIC  X(015).
                   20 DCOMWA1I-S-TEM-NA-AGENCIA   PIC  X(001).
      *-->      ----------------------------------------------------
      *-->  -->    AREA RESERVA PARA EXPANSAO DE DADOS               <--
      *-->      ----------------------------------------------------
                15 DCOMWA1I-S-RESERVA             PIC  X(1249).
      *
