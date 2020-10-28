// ************************************************************************ //
// The types declared in this file were generated from data read from the
// WSDL File described below:
// WSDL     : https://jeans.heineken.com/DISTR4/DRPService.asmx?WSDL
//  >Import : https://jeans.heineken.com/DISTR4/DRPService.asmx?WSDL:0
//  >Import : https://jeans.heineken.com/DISTR4/DRPService.asmx?WSDL:1
// Encoding : utf-8
// Version  : 1.0
// (20.03.2020 17:00:56 - * $Rev: 3108 $)
// ************************************************************************ //

//*13.05.20 заменил тип guid => WideString

unit DRPService;

interface

uses InvokeRegistry, SOAPHTTPClient, Types, XSBuiltIns;

const
  AS_UNBOUNDED = false;

type

  // ************************************************************************ //
  // The following types, referred to in the WSDL document are not being represented
  // in this file. They are either aliases[@] of other types represented or were referred
  // to but never[!] declared in the document. The types from the latter category
  // typically map to predefined/known XML or Borland types; however, they could also 
  // indicate incorrect WSDL documents that failed to declare or import a schema type.
  // ************************************************************************ //
  // !:int             - "http://www.w3.org/2001/XMLSchema"
  // !:double          - "http://www.w3.org/2001/XMLSchema"
  // !:string          - "http://www.w3.org/2001/XMLSchema"
  // !:dateTime        - "http://www.w3.org/2001/XMLSchema"

  DistributorDebet     = class;                 { "http://jeans.heineken.com/DISTR/"[GblCplx] }
  SendStatuses         = class;                 { "http://jeans.heineken.com/DISTR/"[GblCplx] }
  Invoice              = class;                 { "http://jeans.heineken.com/DISTR/"[GblCplx] }
  OrderDelivery        = class;                 { "http://jeans.heineken.com/DISTR/"[GblCplx] }
  DeliveryPosition     = class;                 { "http://jeans.heineken.com/DISTR/"[GblCplx] }
  DistributorDelivery  = class;                 { "http://jeans.heineken.com/DISTR/"[GblCplx] }
  DistributorSalePointDelivery = class;         { "http://jeans.heineken.com/DISTR/"[GblCplx] }
  Balance              = class;                 { "http://jeans.heineken.com/DISTR/"[GblCplx] }

  { "http://jeans.heineken.com/DISTR/"[GblSmpl] }
  Statuses = (formed, accepted, shipped, comments, viewed);

  ArrayOfDistributorDebet = array of DistributorDebet;   { "http://jeans.heineken.com/DISTR/"[GblCplx] }


  // ************************************************************************ //
  // XML       : DistributorDebet, global, <complexType>
  // Namespace : http://jeans.heineken.com/DISTR/
  // ************************************************************************ //
  DistributorDebet = class(TRemotable)
  private
    FContragentID: Integer;
    FDebetSum: Double;
    FDebetLimit: Double;
  published
    property ContragentID: Integer read FContragentID write FContragentID;
    property DebetSum: Double read FDebetSum write FDebetSum;
    property DebetLimit: Double read FDebetLimit write FDebetLimit;
  end;

  ArrayOfSendStatuses = array of SendStatuses;   { "http://jeans.heineken.com/DISTR/"[GblCplx] }
  guid            =  type WideString;      { "http://microsoft.com/wsdl/types/"[GblSmpl] }


  // ************************************************************************ //
  // XML       : SendStatuses, global, <complexType>
  // Namespace : http://jeans.heineken.com/DISTR/
  // ************************************************************************ //
  SendStatuses = class(TRemotable)
  private
    FNumber: WideString;  //guid;  *13.05.20 чтобы убрать xmlns= из <Number xmlns="http://microsoft.com/wsdl/types/">10D41A9F-6928-408E-8ABB-E26073860818</Number>
    FStatus: Statuses;
    FComments: WideString;
  published
    property Number: WideString{guid} read FNumber write FNumber;  //*13.05.20
    property Status: Statuses read FStatus write FStatus;
    property Comments: WideString read FComments write FComments;
  end;

  ArrayOfInvoice = array of Invoice;            { "http://jeans.heineken.com/DISTR/"[GblCplx] }
  ArrayOfOrderDelivery = array of OrderDelivery;   { "http://jeans.heineken.com/DISTR/"[GblCplx] }
  ArrayOfDistributorDelivery = array of DistributorDelivery;   { "http://jeans.heineken.com/DISTR/"[GblCplx] }
  ArrayOfDistributorSalePointDelivery = array of DistributorSalePointDelivery;   { "http://jeans.heineken.com/DISTR/"[GblCplx] }


  // ************************************************************************ //
  // XML       : Invoice, global, <complexType>
  // Namespace : http://jeans.heineken.com/DISTR/
  // ************************************************************************ //
  Invoice = class(TRemotable)
  private
    FInvoiceNum: WideString;
    FInvoiceDate: WideString;
    FOrderDeliveries: ArrayOfOrderDelivery;
    FDistributorDeliveries: ArrayOfDistributorDelivery;
    FDistributorSalePointDeliveries: ArrayOfDistributorSalePointDelivery;
  public
    destructor Destroy; override;
  published
    property InvoiceNum: WideString read FInvoiceNum write FInvoiceNum;
    property InvoiceDate: WideString read FInvoiceDate write FInvoiceDate;
    property OrderDeliveries: ArrayOfOrderDelivery read FOrderDeliveries write FOrderDeliveries;
    property DistributorDeliveries: ArrayOfDistributorDelivery read FDistributorDeliveries write FDistributorDeliveries;
    property DistributorSalePointDeliveries: ArrayOfDistributorSalePointDelivery read FDistributorSalePointDeliveries write FDistributorSalePointDeliveries;
  end;

  ArrayOfDeliveryPosition = array of DeliveryPosition;   { "http://jeans.heineken.com/DISTR/"[GblCplx] }


  // ************************************************************************ //
  // XML       : OrderDelivery, global, <complexType>
  // Namespace : http://jeans.heineken.com/DISTR/
  // ************************************************************************ //
  OrderDelivery = class(TRemotable)
  private
    FDeliveryPositions: ArrayOfDeliveryPosition;
    FOrderID: WideString;
  public
    destructor Destroy; override;
  published
    property DeliveryPositions: ArrayOfDeliveryPosition read FDeliveryPositions write FDeliveryPositions;
    property OrderID: WideString read FOrderID write FOrderID;
  end;



  // ************************************************************************ //
  // XML       : DeliveryPosition, global, <complexType>
  // Namespace : http://jeans.heineken.com/DISTR/
  // ************************************************************************ //
  DeliveryPosition = class(TRemotable)
  private
    FSkuID: WideString;
    FVolume: WideString;
    FRubles: WideString;
    FCount: WideString;
    FComments: WideString;
    FHasPromo: WideString;
    FDiscount: WideString;
  published
    property SkuID: WideString read FSkuID write FSkuID;
    property Volume: WideString read FVolume write FVolume;
    property Rubles: WideString read FRubles write FRubles;
    property Count: WideString read FCount write FCount;
    property Comments: WideString read FComments write FComments;
    property HasPromo: WideString read FHasPromo write FHasPromo;
    property Discount: WideString read FDiscount write FDiscount;
  end;



  // ************************************************************************ //
  // XML       : DistributorDelivery, global, <complexType>
  // Namespace : http://jeans.heineken.com/DISTR/
  // ************************************************************************ //
  DistributorDelivery = class(TRemotable)
  private
    FDeliveryPositions: ArrayOfDeliveryPosition;
    FDistributorOrderID: WideString;
    FSalePointID: WideString;
  public
    destructor Destroy; override;
  published
    property DeliveryPositions: ArrayOfDeliveryPosition read FDeliveryPositions write FDeliveryPositions;
    property DistributorOrderID: WideString read FDistributorOrderID write FDistributorOrderID;
    property SalePointID: WideString read FSalePointID write FSalePointID;
  end;



  // ************************************************************************ //
  // XML       : DistributorSalePointDelivery, global, <complexType>
  // Namespace : http://jeans.heineken.com/DISTR/
  // ************************************************************************ //
  DistributorSalePointDelivery = class(TRemotable)
  private
    FDeliveryPositions: ArrayOfDeliveryPosition;
    FDistributorOrderID: WideString;
    FDistributorSalePointID: WideString;
    FSalePointName: WideString;
    FSalePointAddress: WideString;
    FWarehouseID: WideString;
  public
    destructor Destroy; override;
  published
    property DeliveryPositions: ArrayOfDeliveryPosition read FDeliveryPositions write FDeliveryPositions;
    property DistributorOrderID: WideString read FDistributorOrderID write FDistributorOrderID;
    property DistributorSalePointID: WideString read FDistributorSalePointID write FDistributorSalePointID;
    property SalePointName: WideString read FSalePointName write FSalePointName;
    property SalePointAddress: WideString read FSalePointAddress write FSalePointAddress;
    property WarehouseID: WideString read FWarehouseID write FWarehouseID;
  end;

  ArrayOfBalance = array of Balance;            { "http://jeans.heineken.com/DISTR/"[GblCplx] }


  // ************************************************************************ //
  // XML       : Balance, global, <complexType>
  // Namespace : http://jeans.heineken.com/DISTR/
  // ************************************************************************ //
  Balance = class(TRemotable)
  private
    FSkuID: Integer;
    FWarehouseID: Integer;
    FAmount: Integer;
  published
    property SkuID: Integer read FSkuID write FSkuID;
    property WarehouseID: Integer read FWarehouseID write FWarehouseID;
    property Amount: Integer read FAmount write FAmount;
  end;

  DRP_DeleteReturnResult =  type WideString;      { "http://jeans.heineken.com/DISTR/"[Cplx] }
  DRP_DeleteSellInResult =  type WideString;      { "http://jeans.heineken.com/DISTR/"[Cplx] }
  DRP_DeleteSellOutResult =  type WideString;      { "http://jeans.heineken.com/DISTR/"[Cplx] }
  DRP_GetEncashmentsResult =  type WideString;      { "http://jeans.heineken.com/DISTR/"[Cplx] }
  DRP_GetEncashmentsByDateResult =  type WideString;      { "http://jeans.heineken.com/DISTR/"[Cplx] }
  DRP_GetOrdersResult =  type WideString;      { "http://jeans.heineken.com/DISTR/"[Cplx] }
  DRP_GetOrdersByDateResult =  type WideString;      { "http://jeans.heineken.com/DISTR/"[Cplx] }
  DRP_GetOrdersDemoResult =  type WideString;      { "http://jeans.heineken.com/DISTR/"[Cplx] }
  DRP_GetSalePoints2Result =  type WideString;      { "http://jeans.heineken.com/DISTR/"[Cplx] }
  DRP_GetSalePointsResult =  type WideString;      { "http://jeans.heineken.com/DISTR/"[Cplx] }
  DRP_GetSkuAssortmentResult =  type WideString;      { "http://jeans.heineken.com/DISTR/"[Cplx] }
  DRP_GetWarehousesResult =  type WideString;      { "http://jeans.heineken.com/DISTR/"[Cplx] }
  DRP_SendAllContragentDebetResult =  type WideString;      { "http://jeans.heineken.com/DISTR/"[Cplx] }
  DRP_SendOrdersStatusesResult =  type WideString;      { "http://jeans.heineken.com/DISTR/"[Cplx] }
  DRP_SendReturnResult =  type WideString;      { "http://jeans.heineken.com/DISTR/"[Cplx] }
  DRP_SendSellInResult =  type WideString;      { "http://jeans.heineken.com/DISTR/"[Cplx] }
  DRP_SendSellOutResult =  type WideString;      { "http://jeans.heineken.com/DISTR/"[Cplx] }
  DRP_SendWarehousesBalanceResult =  type WideString;      { "http://jeans.heineken.com/DISTR/"[Cplx] }

  // ************************************************************************ //
  // Namespace : http://jeans.heineken.com/DISTR/
  // soapAction: http://jeans.heineken.com/DISTR/%operationName%
  // transport : http://schemas.xmlsoap.org/soap/http
  // style     : document
  // binding   : DRPServiceSoap
  // service   : DRPService
  // port      : DRPServiceSoap
  // URL       : https://jeans.heineken.com/JDGS/DRPService.asmx
  // ************************************************************************ //
  DRPServiceSoap = interface(IInvokable)
  ['{571869B7-2969-795E-560B-D26ACE8CBE7C}']
    function  GetVersion: WideString; stdcall;
    function  DRP_DeleteReturn(const _login: WideString; const _pass: WideString; const _invoiceDate: WideString; const _invoiceNum: WideString): DRP_DeleteReturnResult; stdcall;
    function  DRP_DeleteSellIn(const _login: WideString; const _pass: WideString; const _invoiceDate: WideString; const _invoiceNum: WideString): DRP_DeleteSellInResult; stdcall;
    function  DRP_DeleteSellOut(const _login: WideString; const _pass: WideString; const _invoiceDate: WideString; const _invoiceNum: WideString): DRP_DeleteSellOutResult; stdcall;
    function  DRP_GetEncashments(const _login: WideString; const _pass: WideString): DRP_GetEncashmentsResult; stdcall;
    function  DRP_GetEncashmentsByDate(const _login: WideString; const _pass: WideString; const date: WideString): DRP_GetEncashmentsByDateResult; stdcall;
    function  DRP_GetOrders(const _login: WideString; const _pass: WideString): DRP_GetOrdersResult; stdcall;
    function  DRP_GetOrdersByDate(const _login: WideString; const _pass: WideString; const _date: TXSDateTime): DRP_GetOrdersByDateResult; stdcall;
    function  DRP_GetOrdersDemo(const _login: WideString; const _pass: WideString): DRP_GetOrdersDemoResult; stdcall;
    function  DRP_GetSalePoints2(const _login: WideString; const _pass: WideString): DRP_GetSalePoints2Result; stdcall;
    function  DRP_GetSalePoints(const _login: WideString; const _pass: WideString): DRP_GetSalePointsResult; stdcall;
    function  DRP_GetSkuAssortment(const _login: WideString; const _pass: WideString): DRP_GetSkuAssortmentResult; stdcall;
    function  DRP_GetWarehouses(const _login: WideString; const _pass: WideString): DRP_GetWarehousesResult; stdcall;
    function  DRP_SendAllContragentDebet(const _login: WideString; const _pass: WideString; const _contragentDebets: ArrayOfDistributorDebet): DRP_SendAllContragentDebetResult; stdcall;
    function  DRP_SendOrdersStatuses(const _login: WideString; const _pass: WideString; const _statuses: ArrayOfSendStatuses): DRP_SendOrdersStatusesResult; stdcall;
    function  DRP_SendReturn(const _login: WideString; const _pass: WideString; const _invoices: ArrayOfInvoice): DRP_SendReturnResult; stdcall;
    function  DRP_SendSellIn(const _login: WideString; const _pass: WideString; const _invoices: ArrayOfInvoice): DRP_SendSellInResult; stdcall;
    function  DRP_SendSellOut(const _login: WideString; const _pass: WideString; const _invoices: ArrayOfInvoice): DRP_SendSellOutResult; stdcall;
    function  DRP_SendWarehousesBalance(const _login: WideString; const _pass: WideString; const _warehouseBalanceData: ArrayOfBalance; const date: TXSDateTime): DRP_SendWarehousesBalanceResult; stdcall;
  end;

function GetDRPServiceSoap(UseWSDL: Boolean=System.False; Addr: string=''; HTTPRIO: THTTPRIO = nil): DRPServiceSoap;


implementation
  uses SysUtils;

function GetDRPServiceSoap(UseWSDL: Boolean; Addr: string; HTTPRIO: THTTPRIO): DRPServiceSoap;
const
  defWSDL = 'https://jeans.heineken.com/DISTR4/DRPService.asmx?WSDL';
  defURL  = 'https://jeans.heineken.com/JDGS/DRPService.asmx';
  defSvc  = 'DRPService';
  defPrt  = 'DRPServiceSoap';
var
  RIO: THTTPRIO;
begin
  Result := nil;
  if (Addr = '') then
  begin
    if UseWSDL then
      Addr := defWSDL
    else
      Addr := defURL;
  end;
  if HTTPRIO = nil then
    RIO := THTTPRIO.Create(nil)
  else
    RIO := HTTPRIO;
  try
    Result := (RIO as DRPServiceSoap);
    if UseWSDL then
    begin
      RIO.WSDLLocation := Addr;
      RIO.Service := defSvc;
      RIO.Port := defPrt;
    end else
      RIO.URL := Addr;
  finally
    if (Result = nil) and (HTTPRIO = nil) then
      RIO.Free;
  end;
end;


destructor Invoice.Destroy;
var
  I: Integer;
begin
  for I := 0 to Length(FOrderDeliveries)-1 do
    FreeAndNil(FOrderDeliveries[I]);
  SetLength(FOrderDeliveries, 0);
  for I := 0 to Length(FDistributorDeliveries)-1 do
    FreeAndNil(FDistributorDeliveries[I]);
  SetLength(FDistributorDeliveries, 0);
  for I := 0 to Length(FDistributorSalePointDeliveries)-1 do
    FreeAndNil(FDistributorSalePointDeliveries[I]);
  SetLength(FDistributorSalePointDeliveries, 0);
  inherited Destroy;
end;

destructor OrderDelivery.Destroy;
var
  I: Integer;
begin
  for I := 0 to Length(FDeliveryPositions)-1 do
    FreeAndNil(FDeliveryPositions[I]);
  SetLength(FDeliveryPositions, 0);
  inherited Destroy;
end;

destructor DistributorDelivery.Destroy;
var
  I: Integer;
begin
  for I := 0 to Length(FDeliveryPositions)-1 do
    FreeAndNil(FDeliveryPositions[I]);
  SetLength(FDeliveryPositions, 0);
  inherited Destroy;
end;

destructor DistributorSalePointDelivery.Destroy;
var
  I: Integer;
begin
  for I := 0 to Length(FDeliveryPositions)-1 do
    FreeAndNil(FDeliveryPositions[I]);
  SetLength(FDeliveryPositions, 0);
  inherited Destroy;
end;

initialization
  InvRegistry.RegisterInterface(TypeInfo(DRPServiceSoap), 'http://jeans.heineken.com/DISTR/', 'utf-8');
  InvRegistry.RegisterDefaultSOAPAction(TypeInfo(DRPServiceSoap), 'http://jeans.heineken.com/DISTR/%operationName%');
  InvRegistry.RegisterInvokeOptions(TypeInfo(DRPServiceSoap), ioDocument);
  RemClassRegistry.RegisterXSInfo(TypeInfo(ArrayOfDistributorDebet), 'http://jeans.heineken.com/DISTR/', 'ArrayOfDistributorDebet');
  RemClassRegistry.RegisterXSClass(DistributorDebet, 'http://jeans.heineken.com/DISTR/', 'DistributorDebet');
  RemClassRegistry.RegisterXSInfo(TypeInfo(ArrayOfSendStatuses), 'http://jeans.heineken.com/DISTR/', 'ArrayOfSendStatuses');
  RemClassRegistry.RegisterXSInfo(TypeInfo(Statuses), 'http://jeans.heineken.com/DISTR/', 'Statuses');
  RemClassRegistry.RegisterXSInfo(TypeInfo(guid), 'http://microsoft.com/wsdl/types/', 'guid');  //-13.05.20 убрать эту хрень из тега с номером заказа <Number xmlns="http://microsoft.com/wsdl/types/">
  RemClassRegistry.RegisterXSClass(SendStatuses, 'http://jeans.heineken.com/DISTR/', 'SendStatuses');
  RemClassRegistry.RegisterXSInfo(TypeInfo(ArrayOfInvoice), 'http://jeans.heineken.com/DISTR/', 'ArrayOfInvoice');
  RemClassRegistry.RegisterXSInfo(TypeInfo(ArrayOfOrderDelivery), 'http://jeans.heineken.com/DISTR/', 'ArrayOfOrderDelivery');
  RemClassRegistry.RegisterXSInfo(TypeInfo(ArrayOfDistributorDelivery), 'http://jeans.heineken.com/DISTR/', 'ArrayOfDistributorDelivery');
  RemClassRegistry.RegisterXSInfo(TypeInfo(ArrayOfDistributorSalePointDelivery), 'http://jeans.heineken.com/DISTR/', 'ArrayOfDistributorSalePointDelivery');
  RemClassRegistry.RegisterXSClass(Invoice, 'http://jeans.heineken.com/DISTR/', 'Invoice');
  RemClassRegistry.RegisterXSInfo(TypeInfo(ArrayOfDeliveryPosition), 'http://jeans.heineken.com/DISTR/', 'ArrayOfDeliveryPosition');
  RemClassRegistry.RegisterXSClass(OrderDelivery, 'http://jeans.heineken.com/DISTR/', 'OrderDelivery');
  RemClassRegistry.RegisterXSClass(DeliveryPosition, 'http://jeans.heineken.com/DISTR/', 'DeliveryPosition');
  RemClassRegistry.RegisterXSClass(DistributorDelivery, 'http://jeans.heineken.com/DISTR/', 'DistributorDelivery');
  RemClassRegistry.RegisterXSClass(DistributorSalePointDelivery, 'http://jeans.heineken.com/DISTR/', 'DistributorSalePointDelivery');
  RemClassRegistry.RegisterXSInfo(TypeInfo(ArrayOfBalance), 'http://jeans.heineken.com/DISTR/', 'ArrayOfBalance');
  RemClassRegistry.RegisterXSClass(Balance, 'http://jeans.heineken.com/DISTR/', 'Balance');
  RemClassRegistry.RegisterXSInfo(TypeInfo(DRP_DeleteReturnResult), 'http://jeans.heineken.com/DISTR/', 'DRP_DeleteReturnResult');
  RemClassRegistry.RegisterXSInfo(TypeInfo(DRP_DeleteSellInResult), 'http://jeans.heineken.com/DISTR/', 'DRP_DeleteSellInResult');
  RemClassRegistry.RegisterXSInfo(TypeInfo(DRP_DeleteSellOutResult), 'http://jeans.heineken.com/DISTR/', 'DRP_DeleteSellOutResult');
  RemClassRegistry.RegisterXSInfo(TypeInfo(DRP_GetEncashmentsResult), 'http://jeans.heineken.com/DISTR/', 'DRP_GetEncashmentsResult');
  RemClassRegistry.RegisterXSInfo(TypeInfo(DRP_GetEncashmentsByDateResult), 'http://jeans.heineken.com/DISTR/', 'DRP_GetEncashmentsByDateResult');
  RemClassRegistry.RegisterXSInfo(TypeInfo(DRP_GetOrdersResult), 'http://jeans.heineken.com/DISTR/', 'DRP_GetOrdersResult');
  RemClassRegistry.RegisterXSInfo(TypeInfo(DRP_GetOrdersByDateResult), 'http://jeans.heineken.com/DISTR/', 'DRP_GetOrdersByDateResult');
  RemClassRegistry.RegisterXSInfo(TypeInfo(DRP_GetOrdersDemoResult), 'http://jeans.heineken.com/DISTR/', 'DRP_GetOrdersDemoResult');
  RemClassRegistry.RegisterXSInfo(TypeInfo(DRP_GetSalePoints2Result), 'http://jeans.heineken.com/DISTR/', 'DRP_GetSalePoints2Result');
  RemClassRegistry.RegisterXSInfo(TypeInfo(DRP_GetSalePointsResult), 'http://jeans.heineken.com/DISTR/', 'DRP_GetSalePointsResult');
  RemClassRegistry.RegisterXSInfo(TypeInfo(DRP_GetSkuAssortmentResult), 'http://jeans.heineken.com/DISTR/', 'DRP_GetSkuAssortmentResult');
  RemClassRegistry.RegisterXSInfo(TypeInfo(DRP_GetWarehousesResult), 'http://jeans.heineken.com/DISTR/', 'DRP_GetWarehousesResult');
  RemClassRegistry.RegisterXSInfo(TypeInfo(DRP_SendAllContragentDebetResult), 'http://jeans.heineken.com/DISTR/', 'DRP_SendAllContragentDebetResult');
  RemClassRegistry.RegisterXSInfo(TypeInfo(DRP_SendOrdersStatusesResult), 'http://jeans.heineken.com/DISTR/', 'DRP_SendOrdersStatusesResult');
  RemClassRegistry.RegisterXSInfo(TypeInfo(DRP_SendReturnResult), 'http://jeans.heineken.com/DISTR/', 'DRP_SendReturnResult');
  RemClassRegistry.RegisterXSInfo(TypeInfo(DRP_SendSellInResult), 'http://jeans.heineken.com/DISTR/', 'DRP_SendSellInResult');
  RemClassRegistry.RegisterXSInfo(TypeInfo(DRP_SendSellOutResult), 'http://jeans.heineken.com/DISTR/', 'DRP_SendSellOutResult');
  RemClassRegistry.RegisterXSInfo(TypeInfo(DRP_SendWarehousesBalanceResult), 'http://jeans.heineken.com/DISTR/', 'DRP_SendWarehousesBalanceResult');

end.