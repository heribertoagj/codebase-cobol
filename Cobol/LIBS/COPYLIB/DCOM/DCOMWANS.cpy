      *================================================================*
      * DESCRICAO  : GERAR CONTRATO ARQUITETURA - BOTAO CONTRATAR      *
      * COPYBOOK   : DCOMWANS - BOOK DE RETORNO DO PROGRAMA FUNCIONAL  *
      * COORDENADOR: DCOM1ANI - GERAR CONTRATO ARQUITETURA             *
      * FLUXO      : DCOMIABM                                          *
      * DATA       : 28/10/2024                                        *
      * AUTOR      : FABIO AUGUSTO FINK - CAPGEMINI                    *
      * COMPONENTE : DCOM - DESCONTO COMERCIAL                         *
      *================================================================*
      *
          05 DCOMWANS-HEADER.
             10 DCOMWANS-COD-LAYOUT    PIC X(008)   VALUE 'DCOMWANS'.
             10 DCOMWANS-TAM-LAYOUT    PIC 9(005)   VALUE 00500.

             10 DCOMWANS-BLOCO-SAIDA.
      *-->   DADOS DO CONTRATO ARQUITETURA
      *-->   =========================================
                15 DCOMWANS-CPSSOA-JURID-CONTR      PIC  9(010).
                15 DCOMWANS-CTPO-CONTR-NEGOC        PIC  9(003).
                15 DCOMWANS-NSEQ-CONTR-NEGOC        PIC  9(010).
      *
      *-->      ----------------------------------------------------
      *-->  -->    AREA RESERVA PARA EXPANSAO DE DADOS               <--
      *-->      ----------------------------------------------------
                15 DCOMWANS-RESERVA                 PIC  X(464).
