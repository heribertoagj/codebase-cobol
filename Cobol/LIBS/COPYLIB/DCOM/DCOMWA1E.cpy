      *================================================================*
      * DESCRICAO  : CONSULTA / MANUTEN��O DA TESTEMUNHA DE UMA OPERA- *
      *              CAO/PROPOSTA DE ANTECIPACAO - LAYOUT DE ENTRADA   *
      * COPYBOOK   : DCOMWA1E - BOOK DE ACESSO AO PROGRAMA FUNCIONAL   *
      * COORDENADOR: DCOM1ALL - CONTA / MANUTENCAO DE TESTEMUNHAS      *
      * FLUXO      : DCOMIABB                                          *
      * DATA       : 18/06/2024                                        *
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
      *                                                                *
      * DCOMWA1E-CBCO         = CODIGO DO BANCO                        *
      *-> OBRIGATORIO         - BRADESCO = 237                         *
      *                                                                *
      * DCOMWA1E-CAG-BCRIA    = CODIGO DA AGENCIA DA OPERACAO/PROPOSTA *
      *-> OBRIGATORIO                                                  *
      *                                                                *
      * DCOMWA1E-NPSSOA-DESC-COML                                      *
      *                       = NUMERO DA PESSOA NO CADASTRO DO DCOM   *
      *-> OBRIGATORIO         - OBRIGATORIO PARA OPCAO 04 E 05         *
      *                                                                *
      * DCOMWA1E-CCPF         = NUMERO/CORPO DO CPF DA TESTEMUNHA      *
      *-> OBRIGATORIO         - OBRIGATORIO PARA OPCAO 02 E 03         *
      * DCOMWA1E-CCTRL-CPF    = CONTROLE DO CPF DA TESTEMINHA          *
      *-> OBRIGATORIO         - OBRIGATORIO PARA OPCAO 02 E 03         *
      *                                                                *
      * DCOMWA1E-IPSSOA-DESC-COML                                      *
      *                       = NOME DA TESTEMUNHA                     *
      *-> OBRIGATORIO         - OBRIGATORIO PARA OPCAO 03              *
      *                                                                *
      * DCOMWA1E-CCLUB        = CLUB DA TESTEMUNHA (SE ESTIVER NO PSDC)*
      *-> OBRIGATORIO         - OBRIGATORIO PARA OPCAO 03              *
      *                                                                *
      * DCOMWA1E-CDOCTO-ID    = CODIGO DO DOCUMENTO DA TESTEMUNHA (RG) *
      *-> OPCIONAL            -                                        *
      *================================================================*
      *
          05 DCOMWA1E-E-HEADER.
             10 DCOMWA1E-COD-LAYOUT    PIC X(008) VALUE 'DCOMWA1E'.
             10 DCOMWA1E-TAM-LAYOUT    PIC 9(005) VALUE 00300.
      *
          05 DCOMWA1E-BLOCO-ENTRADA.
             10 DCOMWA1E-FUNCAO                   PIC  9(002).
             10 DCOMWA1E-CBCO                     PIC  9(003).
             10 DCOMWA1E-CAG-BCRIA                PIC  9(005).
             10 DCOMWA1E-NPSSOA-DESC-COML         PIC  9(009).
             10 DCOMWA1E-CPF                      PIC  9(009).
             10 DCOMWA1E-CCTRL-CPF                PIC  9(002).
             10 DCOMWA1E-IPSSOA-DESC-COML         PIC  X(060).
             10 DCOMWA1E-CCLUB                    PIC  9(010).
             10 DCOMWA1E-CDOCTO-ID                PIC  X(015).
             10 DCOMWA1E-FILLER                   PIC  X(172).
      *
