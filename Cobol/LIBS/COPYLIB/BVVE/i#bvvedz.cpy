      *----------------------------------------------------------------*
      *  I#BVVEDZ - MANUTENCAO DE DOMICILIO BANCARIO - EM MASSA        *
      *  RETORNO DAS SOLICITACOES DE DESTRAVA C/ FORMATACAO DE CNPJ/CPF*
      *  CONVERTIDAS AO VSAM DESTRPAG DO VAREJO ONLINE                 *
      *  FORMATO DATAS: AAAAMMDD                                       *
      *----------------------------------------------------------------*
      *
       01  BVVEDZ-REGISTRO.
           03  BVVEDZ-COD-MSG          PIC X(007).
           03  BVVEDZ-ORIGEM           PIC X(001).
           03  BVVEDZ-NUMCTRL-IF-ENVIO PIC X(020).
           03  BVVEDZ-DADOS-CLI.
               05 BVVEDZ-TP-PESSOA-CLI PIC X(001).
               05 BVVEDZ-CNPJ-CPF      PIC 9(009).
               05 BVVEDZ-FILIAL        PIC 9(004).
               05 FILLER               PIC X(001).
           03  BVVEDZ-TRAVA-RAIZ       PIC X(001).
           03  BVVEDZ-NUMSCG           PIC 9(017).
           03  BVVEDZ-TP-BAIXA         PIC X(001).
           03  BVVEDZ-DT-MOVTO         PIC 9(008).
           03  BVVEDZ-BANDEIRA         PIC 9(003).
           03  BVVEDZ-COD-PROP-ESQ     PIC X(003).
           03  BVVEDZ-AREA-REJEITADOS.
               05  BVVEDZ-ERRMSG       PIC X(008) OCCURS 10.
