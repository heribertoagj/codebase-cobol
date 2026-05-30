      *================================================================*
      *  I#BVVEDU - MANUTENCAO DE DOMICILIO BANCARIO                   *
      *  SOLICITACOES DE DESTRAVAS EFETUADAS PELA AGENCIA (VAREJO)     *
      *----------------------------------------------------------------*
      *  DSNAME       : CB.BACC.VON.DESTRPAG                           *
      *  LRECL ARQUIVO: 1200                                           *
      *  LRECL CHAVE  : 110                                            *
      *----------------------------------------------------------------*
      *  FORMATOS:                                                     *
      *   TIMESTAMP: AAAAMMDDHHMMSSMMMMMM                              *
      *   DATA     : DD.MM.AAAA                                        *
      *   HORA     : HH:MM:SS                                          *
      *   CONTA    : CONTEM O DIGITO DA CONTA NO ULTIMO BYTE DO CAMPO  *
      *================================================================*
       01  BVVEDU-DESTRPAG.
           05 BVVEDU-CHAVE.
               10 BVVEDU-CH-REGIONAL           PIC  9(005).
               10 BVVEDU-CH-AGENCIA            PIC  9(004).
               10 BVVEDU-CH-DADOS-CLI.
                  15 BVVEDU-CH-CNPJ-CPF        PIC  9(009).
                  15 BVVEDU-CH-FILIAL          PIC  9(004).
               10 BVVEDU-CH-BANDEIRA           PIC  9(003).
               10 BVVEDU-CH-TIMESTAMP          PIC  X(020).
               10 BVVEDU-CH-FILLER             PIC  X(065).
           05 BVVEDU-NUM-IDENT-SCG             PIC  9(017).
           05 BVVEDU-CNPJ-CPF-CTRL             PIC  9(002).
           05 BVVEDU-COD-PROP-ESQM             PIC  X(003).
           05 BVVEDU-TP-PSSOA                  PIC  X(001).
           05 BVVEDU-STATUS                    PIC  9(002).
      *         01 = AGUARDANDO PARECER VAREJO
      *         02 = AGUARDANDO VALIDACAO REGIONAL
      *         03 = APROVADO PELA REGIONAL
      *         04 = REJEITADO PELA REGIONAL
      *         05 = DESTRAVA ENVIADA A CIP
      *         06 = DESTRAVA NAO ACATADA PELA CIP
      *         07 = DESTRAVA EFETUADA COM SUCESSO
           05 BVVEDU-TRAVOU-RAIZ               PIC  X(001).
      *  AGENCIA E CONTA CADASTRADA NO VSAM TRAVPATI (TRAVAS ATIVAS)
           05 BVVEDU-AGENCIA-TRV               PIC  9(004).
           05 BVVEDU-CONTA-TRV                 PIC  9(013).
           05 BVVEDU-VR-SALDO-DISP             PIC S9(013)V99.
           05 BVVEDU-VR-ANTECIPACOES           PIC S9(013)V99.
           05 BVVEDU-BCO-PERDEU                PIC  9(003).
           05 BVVEDU-MOTIVO-DESTR-1            PIC  X(075).
           05 BVVEDU-MOTIVO-DESTR-2            PIC  X(075).
           05 BVVEDU-MOTIVO-DESTR-3            PIC  X(075).
           05 BVVEDU-USUARIO-AG                PIC  X(009).
           05 BVVEDU-SIT-PARECER               PIC  X(001).
      *         P = PENDENTE
      *         E = EFETUADO
      *         N = NAO NECESSARIO
           05 BVVEDU-DS-PARECER-1              PIC  X(075).
           05 BVVEDU-DS-PARECER-2              PIC  X(075).
           05 BVVEDU-DT-PARECER                PIC  X(010).
           05 BVVEDU-HR-PARECER                PIC  X(008).
           05 BVVEDU-APROV-REGIONAL            PIC  X(001).
      *         P = PENDENTE
      *         S = SIM
      *         N = NAO
           05 BVVEDU-DS-REGIONAL-1             PIC  X(075).
           05 BVVEDU-DS-REGIONAL-2             PIC  X(075).
           05 BVVEDU-DT-REGIONAL               PIC  X(010).
           05 BVVEDU-HR-REGIONAL               PIC  X(008).
           05 BVVEDU-DT-ENVIO-CIP              PIC  X(010).
           05 BVVEDU-HR-ENVIO-CIP              PIC  X(008).
           05 BVVEDU-DT-RETORNO-CIP            PIC  X(010).
           05 BVVEDU-HR-RETORNO-CIP            PIC  X(008).
           05 BVVEDU-ERROS-CIP.
              10 BVVEDU-COD-ERRO               PIC  X(008) OCCURS 20.
           05 FILLER                           PIC  X(246).
