      *================================================================*
      * DESCRICAO  : CONSULTAR AGENCIA UORG (DADOS GERAIS)             *
      * COPYBOOK   : DCOMWA2E - BOOK DE ACESSO AO PROGRAMA FUNCIONAL   *
      * COORDENADOR: DCOM1ANC - CONSULTAR DADOS DA AGENCIA NO UORG     *
      * FLUXO      : DCOMIABN                                          *
      * DATA       : 05/11/2024                                        *
      * AUTOR      : FABIO AUGUSTO FINK - CAPGEMINI                    *
      * COMPONENTE : DCOM - DESCONTO COMERCIAL                         *
      *================================================================*
      *
          05 DCOMWA2E-E-HEADER.
             10 DCOMWA2E-COD-LAYOUT    PIC X(008)  VALUE 'DCOMWA2E'.
             10 DCOMWA2E-TAM-LAYOUT    PIC 9(005)  VALUE 00200.
      *
          05 DCOMWA2E-BLOCO-ENTRADA.
      *-->   DADOS DO BANCO/AGENCIA PARA CONSULTA
      *-->   =========================================
             10 DCOMWA2E-CBCO                      PIC  9(003).
             10 DCOMWA2E-CAG-BCRIA                 PIC  9(005).
      *
      *-->   AREA RESERVADA PARA EXPANSAO
      *-->   =========================================
             10 DCOMWA2E-FILLER                    PIC  X(179).
      *
