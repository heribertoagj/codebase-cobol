      *================================================================*
      * DESCRICAO  : INCLUIR OPERACAO DE DESCONTO - BOTAO CONTRATAR    *
      * COPYBOOK   : DCOMWAMS - BOOK DE RETORNO DO PROGRAMA FUNCIONAL  *
      * COORDENADOR: DCOM1AMI - CONTRATACAO DA OPERACAO                *
      * FLUXO      : DCOMIABL                                          *
      * DATA       : 10/10/2024                                        *
      * AUTOR      : LUCIANDRA SILVEIRA                                *
      * COMPONENTE : DCOM - DESCONTO COMERCIAL                         *
      *================================================================*
      *
          05 DCOMWAMS-HEADER.
             10 DCOMWAMS-COD-LAYOUT    PIC X(008)   VALUE 'DCOMWAMS'.
             10 DCOMWAMS-TAM-LAYOUT    PIC 9(005)   VALUE 01900.

             10 DCOMWAMS-BLOCO-SAIDA.
      *-->   DADOS DA OPERACAO CADASTRADA
      *-->   =========================================
                15 DCOMWAMS-DANO-OPER-DESC          PIC  9(004).
                15 DCOMWAMS-NSEQ-OPER-DESC          PIC  9(009).
                15 DCOMWAMS-CSIT-DESC-COML          PIC  9(003).
                15 DCOMWAMS-ISIT-DESC-COML          PIC  X(040).
      *
      *-->   DADOS DO CEDENTE/PROPONENTE DA OPERACAO
      *-->   =========================================
                15 DCOMWAMS-NPSSOA-DESC-COML        PIC  9(009).
                15 DCOMWAMS-NRELAC-PSSOA-DESC       PIC  9(009).
      *
      *-->   DADOS DAS TESTEMUNHAS DA OPERACAO
      *-->   =========================================
                15 DCOMWAME-TESTEMUNHAS             OCCURS 002 TIMES.
                   20 DCOMWAMS-NPSSOA-DESC-TSTM     PIC  9(009).
                   20 DCOMWAMS-NRELAC-PSSOA-TSTM    PIC  9(009).
                   20 DCOMWAMS-CCNPJ-CPF-TSTM       PIC  9(009).
                   20 DCOMWAMS-CCTRL-CNPJ-CPF-TSTM  PIC  9(002).
      *
      *-->      ----------------------------------------------------
      *-->  -->    AREA RESERVA PARA EXPANSAO DE DADOS               <--
      *-->      ----------------------------------------------------
                15 DCOMWAMS-RESERVA                 PIC  X(1755).
